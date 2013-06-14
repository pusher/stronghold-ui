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

import Control.Applicative (empty)
import Control.Lens.TH

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
import Config (githubKeys, authorized)

data StrongholdApp = StrongholdApp {
  _sess :: Snaplet SessionManager
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
            li ! class_ "active" $ a ! href "#" $ "Home"
            li $ a ! href "#about" $ "About"
            li $ a ! href "#contact" $ "Contact"

rootTemplate :: H.Html -> H.Html
rootTemplate maincontent = docTypeHtml $ do
  H.head $ do
    H.title "Stronghold"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "/assets/css/bootstrap.min.css"
    H.style "body { padding-top: 60px; }" -- Don't let the main content overlap the top bar
  H.body $ do
    navbar
    H.div ! class_ "container" $ maincontent

homeTemplate :: H.Html
homeTemplate =
  rootTemplate $
    H.div ! class_ "row" $ do
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
          h3 "Current hierarchy"

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
    ("/:version/hierarchy", showHierarchy),
    ("/:version/message", addMessage),
    ("/:version/update", update),
    ("/assets", serveDirectory "./assets")
   ]
  return $ StrongholdApp session
 where
  home :: Handler StrongholdApp StrongholdApp ()
  home = ifTop $ Snap.method GET $ forceLogin $ do
    -- lookup the head version
    let version = "hello"
    redirect $ B.concat ["/", version, "/info"]

  at :: Handler StrongholdApp StrongholdApp ()
  at = ifTop $ Snap.method GET $ forceLogin $ do
    -- lookup the correct version
    let version = "hello"
    redirect $ B.concat ["/", version, "/info"]

  info :: Handler StrongholdApp StrongholdApp ()
  info = ifTop $ Snap.method GET $ forceLogin $ do
    Just version <- getParam "version"
    writeLazyText $ renderHtml homeTemplate

  showHierarchy :: Handler StrongholdApp StrongholdApp ()
  showHierarchy = undefined

  addMessage :: Handler StrongholdApp StrongholdApp ()
  addMessage = Snap.method POST $ forceLogin $ do
    -- get the hierarchy path out of the path
    -- fetch the json out of the params
    -- show the change
    -- render a form to enter a message and submit
    undefined

  update :: Handler StrongholdApp StrongholdApp ()
  update = undefined

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
    redirect "/"

main :: IO ()
main = serveSnaplet defaultConfig appInit
