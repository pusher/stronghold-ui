{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Tree
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (foldl')

import Control.Applicative (empty)
import Control.Lens.TH
import Control.Monad (mzero)

import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap
import Snap.Util.FileServe

import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Database.Stronghold as S

import qualified Network.OAuth.OAuth2 as OAuth2
import Network.OAuth.OAuth2.HttpClient (doJSONPostRequest)

import Github
import Config (githubKeys, authorized, strongholdURL)

data StrongholdApp = StrongholdApp {
  _sess :: Snaplet SessionManager,
  _stronghold :: S.Client
}
makeLenses ''StrongholdApp -- This is a little bit magic.

navbar :: H.Html
navbar =
  H.div ! class_ "navbar navbar-inverse navbar-fixed-top" $
    H.div ! class_ "navbar-inner" $
      H.div ! class_ "container" $ do
        button !
          type_ "button" !
          class_ "btn btn-navbar" !
          dataAttribute "toggle" "collapse" !
          dataAttribute "target" ".nav-collapse" $ do
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
            H.span ! class_ "icon-bar" $ mempty
        a ! class_ "brand" ! href "#" $ "Stronghold"
        H.div ! class_ "nav-collapse collapse" $
          ul ! class_ "nav" $ do
            li $ a ! href "/" $ "Head"

rootTemplate :: H.Html -> H.Html -> H.Html
rootTemplate headContent mainContent = docTypeHtml $ do
  H.head $ do
    H.title "Stronghold"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/assets/css/bootstrap.min.css"
    H.script ! src "http://code.jquery.com/jquery.js" $ return ()
    H.script ! src "/assets/js/bootstrap.min.js" $ return ()
    H.style "body { padding-top: 60px; }" -- Don't let the main content overlap the top bar
    headContent
  H.body $ do
    navbar
    H.div ! class_ "container" $ mainContent

nodeTemplate :: S.Version -> S.Path -> Aeson.Value -> Aeson.Value -> H.Html
nodeTemplate version path peculiar materialized =
  rootTemplate
    (do
      H.script ! src "/assets/jsoneditor/jsoneditor-min.js" $ return ()
      H.script ! src "/assets/js/node.js" $ return ()
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "/assets/jsoneditor/jsoneditor-min.css")
    (H.div ! class_ "row" $
      H.div ! class_ "span8" $ do
        H.div ! class_ "page-header" $ H.h4 $ toMarkup $ S.pathToText path
        H.p $
          H.div ! A.id "mode" ! A.class_ "btn-group" ! H.dataAttribute "toggle" "buttons-radio" $ do
            H.button ! A.class_ "btn btn-primary active" ! A.value "0" $ "Peculiar"
            H.button ! A.class_ "btn btn-primary" ! A.value "1" $ "Materialized"
        H.div
          ! A.id "peculiar-editor"
          ! dataAttribute "json" (jsonToAttr peculiar)
            $ return ()
        H.div
          ! A.id "materialized-view"
          ! dataAttribute "json" (jsonToAttr materialized)
          ! A.style "display: none"
            $ return ()
        H.br
        let formAction = toValue $ Text.concat ["/", S.versionToText version, "/update", S.pathToText path]
        H.form ! A.id "update-form" ! A.method "POST" ! A.action formAction $
          H.fieldset $ do
            H.input ! A.id "json-field" ! A.type_ "hidden" ! name "json"
            H.textarea ! A.class_ "input-block-level" ! name "comment" ! placeholder "Comment" $ return ()
            H.input ! A.type_ "submit" ! A.value "Update" ! A.class_ "btn")
 where
  jsonToAttr = toValue . decodeUtf8 . B.concat . BL.toChunks . Aeson.encode

versionTemplate :: H.Html -> H.Html
versionTemplate tree =
  rootTemplate
    (do
      H.script ! src "/assets/js/version.js" $ return ()
      H.link ! rel "stylesheet" ! type_ "text/css" ! href "/assets/css/hierarchy.css")
    (H.div ! class_ "row" $ do
      H.div ! class_ "span6" $ do
        h3 "Updates"
      H.div ! class_ "span6" $ do
        H.div $ do
          h3 "Hierarchy At"
          H.form ! A.action "/at" $ do
            H.input ! type_ "date" ! name "date"
            H.input ! type_ "time" ! name "time"
            button ! type_ "submit" $ "Go"
        H.div $ do
          h3 "Hierarchy"
          H.div ! A.class_ "tree" $ tree
        H.div $ do
          H.form ! A.id "navigate-hierarchy" ! A.class_ "form-inline" $ do
            H.input ! type_ "text" ! placeholder "Enter Path" ! name "path"
            button ! type_ "submit" ! A.class_ "btn" $ "Go")

constructTree :: S.Path -> [S.Path] -> Tree S.Path
constructTree root =
  Node root . fmap f . groupByFirstLabel
 where
  f (label, subpaths) = constructTree (root `mappend` S.singletonPath label) subpaths

  groupByFirstLabel :: [S.Path] -> [(Text, [S.Path])]
  groupByFirstLabel =
    HashMap.toList .
    foldl' (HashMap.unionWith (++)) HashMap.empty .
    catMaybes .
    fmap (fmap (\(x, xs) -> HashMap.insert x [xs] HashMap.empty) . S.viewl)

renderTree :: S.Version -> Tree S.Path -> Html
renderTree version tree =
  H.ul $ renderTreeLi version tree
 where
  renderTreeLi :: S.Version -> Tree S.Path -> H.Html
  renderTreeLi version (Node x children) = do
    H.li $ do
      let x' = S.pathToText x
      H.a ! href (toValue (Text.concat ["/", S.versionToText version, "/node", x'])) $
        case S.pathToList x of
          [] -> "root"
          l -> toMarkup $ last l
      H.ul $ mapM_ (renderTreeLi version) children

appInit :: SnapletInit StrongholdApp StrongholdApp
appInit = makeSnaplet "stronghold" "The management UI for stronghold" Nothing $ do
  session <- nestSnaplet "" sess $
    initCookieSessionManager "config/session_secret" "sess" Nothing
  addRoutes [
    ("/", home),
    ("/at", at),
    ("/login", login),
    ("/auth/github/callback", githubCallback),
    ("/logout", logout),
    ("/:version/info", info),
    ("/:version/node", node),
    ("/:version/update", updateNode),
    ("/assets", serveDirectory "./assets")
   ]
  client <- liftIO $ S.newClient strongholdURL
  return $ StrongholdApp session client
 where
  home :: Handler StrongholdApp StrongholdApp ()
  home = ifTop $ Snap.method GET $ forceLogin $ do
    client <- gets _stronghold
    version <- liftIO $ S.headRef client
    redirect $ B.concat ["/", S.versionToBS version, "/info"]

  at :: Handler StrongholdApp StrongholdApp ()
  at = ifTop $ Snap.method GET $ forceLogin $ do
    client <- gets _stronghold
    version <- liftIO $ S.at client undefined
    redirect $ B.concat ["/", S.versionToBS version, "/info"]

  -- The page for a particular version
  info :: Handler StrongholdApp StrongholdApp ()
  info = ifTop $ Snap.method GET $ forceLogin $ do
    Just version <- fmap S.bsToVersion <$> getParam "version"
    client <- gets _stronghold
    paths <- liftIO $ S.paths client version
    let tree = constructTree mempty paths
    writeLazyText $ renderHtml $ versionTemplate $ renderTree version tree

  getPath :: Handler StrongholdApp StrongholdApp [Text]
  getPath = do
    path <- (decodeUtf8 . rqPathInfo) <$> getRequest
    if Text.null path then
      return mempty
     else if Text.last path == '/' then
      fail "couldn't construct path"
     else
      (return . Text.splitOn "/") path

  -- The page for a particular node
  node :: Handler StrongholdApp StrongholdApp ()
  node = Snap.method GET $ forceLogin $ do
    Just version <- getParam "version"
    let version' = S.bsToVersion version
    path <- S.listToPath <$> getPath
    client <- gets _stronghold
    peculiar <- liftIO $ S.peculiar client version' path
    materializedParent <-
      maybe
        (return (Aeson.object []))
        (liftIO . S.materialized client version')
        (S.parent path)
    writeLazyText $ renderHtml $ nodeTemplate version' path peculiar materializedParent

  updateNode :: Handler StrongholdApp StrongholdApp ()
  updateNode = Snap.method POST $ forceLogin $ do
    Just version <- getParam "version"
    path <- S.listToPath <$> getPath
    comment <- getParam "comment"
    json <- getParam "json"
    author <- with sess $ getFromSession "author"
    case (,,) <$> (json >>= (Aeson.decode . BL.fromChunks . return)) <*> author <*> comment of
      Nothing -> writeText "bad params"
      Just (json', author', comment') -> do
        client <- gets _stronghold
        let version' = S.bsToVersion version
        let comment'' = decodeUtf8 comment'
        result <- liftIO $ S.updatePath client version' path json' author' comment''
        case result of
          Right result' -> redirect (B.concat ["/", (encodeUtf8 . S.versionToText) result', "/info"])
          Left err -> writeText $ Text.concat ["failed: ", err]

  loggedIn :: Handler StrongholdApp StrongholdApp Bool
  loggedIn =
    isJust <$> (with sess $ getFromSession "author")

  forceLogin :: Handler StrongholdApp StrongholdApp () -> Handler StrongholdApp StrongholdApp ()
  forceLogin continue = do
    x <- loggedIn
    if x then
      continue
     else
      redirect "/login"

  login :: Handler StrongholdApp StrongholdApp ()
  login = Snap.method GET $ ifTop $ redirect (OAuth2.authorizationUrl githubKeys)

  githubCallback :: Handler StrongholdApp StrongholdApp ()
  githubCallback = Snap.method GET $ ifTop $ do
    code <- getParam "code"
    case code of
      Nothing -> empty
      Just code' -> do
        let (url, body) = OAuth2.accessTokenUrl githubKeys code'
        token <- liftIO $ doJSONPostRequest (url, body)
        case token of
          Nothing -> writeText "invalid token"
          Just (OAuth2.AccessToken token' _) -> do
            user <- liftIO $ userInfo (githubKeys {OAuth2.oauthAccessToken = Just token'})
            case user of
              Nothing -> writeText "not authorized"
              Just user'@(GithubUser _ name email) ->
                if authorized user' then do
                  with sess $ do
                    setInSession "author" (Text.concat [name, " <", email, ">"])
                    commitSession
                  redirect "/"
                else
                  writeText "not authorized"

  logout :: Handler StrongholdApp StrongholdApp ()
  logout = Snap.method GET $ ifTop $ do
    with sess $ do
      resetSession
      commitSession
    writeText "logged out"

main :: IO ()
main = serveSnaplet defaultConfig appInit
