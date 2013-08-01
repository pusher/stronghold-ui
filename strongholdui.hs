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
import Data.List (foldl', intersperse)
import Data.Time.Clock
import Data.Configurator (load, require, Worth(Required))
import Data.Configurator.Types (Configured, convert, Value(List))

import Control.Applicative (empty)
import Control.Lens (set)
import Control.Lens.TH
import Control.Monad (mzero)

import System.Environment (getArgs)

import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap
import Snap.Util.FileServe

import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Database.Stronghold as S

import qualified Network.OAuth.OAuth2 as OAuth2
import Network.OAuth.OAuth2.HttpClient (doJSONPostRequest)

import Github

data StrongholdApp = StrongholdApp {
  _sess :: Snaplet SessionManager,
  _stronghold :: S.Client,
  _storedHead :: Maybe S.Version
}
makeLenses ''StrongholdApp -- This is a little bit magic.

data AppConfig = AppConfig {
  strongholdURL :: String,
  githubKeys :: OAuth2.OAuth2,
  authorised :: GithubUser -> Bool,
  portNum :: Int,
  sessionSecretPath :: FilePath
}

navbar :: H.Html
navbar =
  H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
    H.div ! A.class_ "navbar-inner" $
      H.div ! A.class_ "container" $ do
        H.button !
          A.type_ "H.button" !
          A.class_ "btn btn-navbar" !
          H.dataAttribute "toggle" "collapse" !
          H.dataAttribute "target" ".nav-collapse" $ do
            H.span ! A.class_ "icon-bar" $ mempty
            H.span ! A.class_ "icon-bar" $ mempty
            H.span ! A.class_ "icon-bar" $ mempty
        H.a ! A.class_ "brand" ! A.href "#" $ "Stronghold"
        H.div ! A.class_ "nav-collapse collapse" $
          H.ul ! A.class_ "nav" $ do
            H.li $ H.a ! A.href "/" $ "Head"

rootTemplate :: H.Html -> H.Html -> H.Html
rootTemplate headContent mainContent = H.docTypeHtml $ do
  H.head $ do
    H.title "Stronghold"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/css/bootstrap.min.css"
    H.script ! A.src "http://code.jquery.com/jquery.js" $ return ()
    H.script ! A.src "/assets/js/bootstrap.min.js" $ return ()
    H.style "body { padding-top: 60px; }" -- Don't let the main content overlap the top bar
    headContent
  H.body $ do
    navbar
    H.div ! A.class_ "container" $ mainContent

nodeTemplate :: S.Version -> S.Path -> Aeson.Value -> Aeson.Value -> H.Html
nodeTemplate version path peculiar materialized =
  rootTemplate
    (do
      H.script ! A.src "/assets/jsoneditor/jsoneditor-min.js" $ return ()
      H.script ! A.src "/assets/js/node.js" $ return ()
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/jsoneditor/jsoneditor-min.css")
    (H.div ! A.class_ "row" $
      H.div ! A.class_ "span8" $ do
        H.div ! A.class_ "page-header" $ H.h4 $ H.toMarkup $ S.pathToText path
        H.p $
          H.div ! A.id "mode" ! A.class_ "btn-group" ! H.dataAttribute "toggle" "H.buttons-radio" $ do
            H.button ! A.class_ "btn btn-primary active" ! A.value "0" $ "Peculiar"
            H.button ! A.class_ "btn btn-primary" ! A.value "1" $ "Materialized"
        H.div
          ! A.id "peculiar-editor"
          ! H.dataAttribute "json" (jsonToAttr peculiar)
            $ return ()
        H.div
          ! A.id "materialized-view"
          ! H.dataAttribute "json" (jsonToAttr materialized)
          ! A.style "display: none"
            $ return ()
        H.br
        let formAction = H.toValue $ Text.concat ["/", S.versionToText version, "/update", S.pathToText path]
        H.form ! A.id "update-form" ! A.method "POST" ! A.action formAction $
          H.fieldset $ do
            H.input ! A.id "json-field" ! A.type_ "hidden" ! A.name "json"
            H.textarea ! A.class_ "input-block-level" ! A.name "comment" ! A.placeholder "Comment" $ return ()
            H.input ! A.type_ "submit" ! A.value "Update" ! A.class_ "btn")
 where
  jsonToAttr = H.toValue . decodeUtf8 . B.concat . BL.toChunks . Aeson.encode

humanizeTime :: UTCTime -> UTCTime -> Text
humanizeTime now ts =
  let diff = now `diffUTCTime` ts
      minute = 60
      hour = 60*minute
      day = 24*hour
      week = 7*day
      month = 30*day
      year = 365*day in
    if diff < 5 then
      "just now"
    else if diff < minute then
      Text.concat [Text.pack $ show $ round diff, " seconds ago"]
    else if diff < hour then
      case round $ diff/minute of
        1 -> "a minute ago"
        n -> Text.concat [Text.pack $ show n, " minutes ago"]
    else if diff < day then
      case round $ diff/hour of
        1 -> "an hour ago"
        n -> Text.concat [Text.pack $ show n, " hours ago"]
    else if diff < week then
      case round $ diff/day of
        1 -> "a day ago"
        n -> Text.concat [Text.pack $ show n, " days ago"]
    else if diff < month then
      case round $ diff/week of
        1 -> "a week ago"
        n -> Text.concat [Text.pack $ show n, " weeks ago"]
    else if diff < year then
      case round $ diff/month of
        1 -> "a month ago"
        n -> Text.concat [Text.pack $ show n, " months ago"]
    else
      case round $ diff/year of
        1 -> "a year ago"
        n -> Text.concat [Text.pack $ show n, " years ago"]

makeTimeHTML :: UTCTime -> UTCTime -> H.Html
makeTimeHTML now ts =
  let ts' = (H.toValue . Text.pack . show) ts in
    H.time ! A.datetime ts' ! A.title ts' $ H.toMarkup (humanizeTime now ts)

type VersionsInfo = [(S.Version, S.MetaInfo, [S.Path])]
type UpdateInfo = ((S.Version, S.MetaInfo, [S.Path]), [S.Change])

versionTemplate :: UTCTime -> S.Version -> H.Html -> Maybe (VersionsInfo, UpdateInfo) -> VersionsInfo -> H.Html
versionTemplate now version tree upto after =
  rootTemplate
    (do
      H.script ! A.src "/assets/js/version.js" $ return ()
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/css/hierarchy.css")
    (H.div ! A.class_ "row" $ do
      H.div ! A.class_ "span6" $ do
        H.h3 "Updates"
        when (not (null after)) $ do
          forM_ (reverse after) renderShortVersion
          H.div ! A.id "newer-wrapper" $
            H.div ! A.id "newer" $ "newer ⇀"
        case upto of
          Just (before, (current, _)) -> do
            renderShortVersion current ! A.id "current-change"
            when (not (null before)) $ do
              H.div ! A.id "older-wrapper-wrapper" $
                H.div ! A.id "older-wrapper" $
                  H.div ! A.id "older" $ "↼ older"
              forM_ before renderShortVersion
          Nothing -> return ()
      H.div ! A.class_ "span6" $ do
        H.div $ do
          H.h3 "Hierarchy"
          H.div ! A.class_ "tree" $ tree
          H.form ! A.id "navigate-hierarchy" ! A.class_ "form-inline" $ do
            H.input ! A.type_ "text" ! A.placeholder "Enter Path" ! A.name "path"
            H.button ! A.type_ "submit" ! A.class_ "btn" $ "Go")
 where
  renderPaths :: [S.Path] -> Text
  renderPaths [] = ""
  renderPaths [x] = S.pathToText x
  renderPaths x =
    let x' = map S.pathToText x
        r = reverse x'
        l = head r
        o = reverse (tail r) in
          Text.concat $ intersperse ", " o ++ [" and ", l]

  renderShortVersion :: (S.Version, S.MetaInfo, [S.Path]) -> H.Html
  renderShortVersion (version, S.MetaInfo ts comment author, paths) =
    H.div ! A.class_ "short-change" $ do
      makeTimeHTML now ts
      H.a ! (A.href $ H.toValue $ Text.concat ["/", S.versionToText version, "/info"]) $
        H.h5 $ H.toMarkup $ Text.concat [author, " updated ", renderPaths paths]
      H.toMarkup comment

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

renderTree :: S.Version -> Tree S.Path -> H.Html
renderTree version tree =
  H.ul $ renderTreeLi version tree
 where
  renderTreeLi :: S.Version -> Tree S.Path -> H.Html
  renderTreeLi version (Node x children) = do
    H.li $ do
      let x' = S.pathToText x
      H.a ! A.href (H.toValue (Text.concat ["/", S.versionToText version, "/node", x'])) $
        case S.pathToList x of
          [] -> "root"
          l -> H.toMarkup $ last l
      H.ul $ mapM_ (renderTreeLi version) children

appInit :: AppConfig -> SnapletInit StrongholdApp StrongholdApp
appInit (AppConfig strongholdURL githubKeys authorised _ sessionSecretPath) =
  makeSnaplet "stronghold" "The management UI for stronghold" Nothing $ do
    session <- nestSnaplet "" sess $
      initCookieSessionManager sessionSecretPath "sess" Nothing
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
    return $ StrongholdApp session client Nothing
 where
  getHead :: Handler StrongholdApp StrongholdApp S.Version
  getHead = do
    head <- gets _storedHead
    case head of
      Just head' -> return head'
      Nothing -> do
        client <- gets _stronghold
        version <- liftIO $ S.headRef client
        modify (set storedHead (Just version))
        return version

  home :: Handler StrongholdApp StrongholdApp ()
  home = ifTop $ Snap.method GET $ forceLogin $ do
    version <- getHead
    redirect $ B.concat ["/", S.versionToBS version, "/info"]

  at :: Handler StrongholdApp StrongholdApp ()
  at = ifTop $ Snap.method GET $ forceLogin $ do
    client <- gets _stronghold
    version <- liftIO $ S.at client undefined
    redirect $ B.concat ["/", S.versionToBS version, "/info"]

  -- The page for a particular version
  info :: Handler StrongholdApp StrongholdApp ()
  info = ifTop $ Snap.method GET $ forceLogin $ do
    modifyResponse $ setContentType "text/html; charset=utf-8"
    Just version <- fmap S.bsToVersion <$> getParam "version"
    client <- gets _stronghold
    storedHead <- getHead
    b <- reverse <$> (liftIO $ S.before client version (Just 10))
    a <- liftIO $ S.after client version storedHead 10
    paths <- liftIO $ S.paths client version
    let tree = constructTree mempty paths
    now <- liftIO $ getCurrentTime
    info <- liftIO $ S.fetchVersionInfo client version
    let upto =
          flip fmap info (\(meta, changes) ->
            let paths = map (\(S.Change path _ _) -> path) changes in
              (drop 1 b, ((version, meta, paths), changes)))
    writeLazyText $ renderHtml $
      versionTemplate now version (renderTree version tree) upto a

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
    modifyResponse $ setContentType "text/html; charset=utf-8"
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
              Nothing -> writeText "not authorised"
              Just user'@(GithubUser _ name _ _) ->
                if authorised user' then do
                  with sess $ do
                    setInSession "author" name
                    commitSession
                  redirect "/"
                else
                  writeText "not authorised"

  logout :: Handler StrongholdApp StrongholdApp ()
  logout = Snap.method GET $ ifTop $ do
    with sess $ do
      resetSession
      commitSession
    writeText "logged out"

convertList :: Configured a => Value -> Maybe [a]
convertList (List x) = sequence (map convert x)
convertList _ = Nothing

fetchConfig :: String -> IO AppConfig
fetchConfig filename = do
  config <- load [Required filename]
  strongholdURL <- require config "stronghold-url"
  clientID <- require config "github-client-id"
  clientSecret <- require config "github-client-secret"
  authorised <- convertList <$> require config "authorised-users"
  authorised' <- maybe (error "expected a list of github usernames") return authorised
  portNum <- require config "port"
  sessionSecretPath <- require config "session-secret-path"
  return
    (AppConfig
      strongholdURL
      (OAuth2.OAuth2
        clientID
        clientSecret
        "https://github.com/login/oauth/authorize"
        "https://github.com/login/oauth/access_token"
        Nothing
        Nothing)
      (flip elem authorised' . githubLogin)
      portNum
      sessionSecretPath)

main :: IO ()
main = do
  [configFile] <- getArgs
  config <- fetchConfig configFile
  serveSnaplet (setPort (portNum config) defaultConfig) (appInit config)
