{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Data.Maybe ( isJust )
import Data.Monoid ( Monoid(mconcat, mempty), Endo(Endo, appEndo) )
import Data.Text ( Text )
import qualified Data.Text as Text ( splitOn, null, last, concat, pack )
import qualified Data.Text.IO as TIO ( hPutStrLn )
import Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import qualified Data.ByteString as B ( concat, ByteString(..) )
import qualified Data.ByteString.Char8 as BC ( hPutStrLn )
import qualified Data.ByteString.Lazy as BL ( fromChunks )
import qualified Data.Aeson as Aeson ( object, decode )
import Data.Time.Clock ( getCurrentTime )
import Data.Configurator ( load, require, Worth(Required) )
import Data.Configurator.Types ( Configured, convert, Value(List) )
import Control.Applicative ( Alternative(empty) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State.Class ( gets, modify )
import Control.Lens ( set )
import Control.Exception ( try, SomeException )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(..), exitWith )
import Snap.Snaplet.Session
    ( setInSession, resetSession, getFromSession, commitSession )
import Snap.Snaplet.Session.Backends.CookieSession
    ( initCookieSessionManager )
import Snap
    ( Snap,
      Handler,
      SnapletInit,
      ConfigLog(ConfigIoLog),
      Config,
      Request(rqPathInfo),
      Method(GET, POST),
      MonadSnaplet(with),
      setPort,
      setErrorLog,
      setAccessLog,
      defaultConfig,
      simpleHttpServe,
      writeText,
      writeLazyText,
      redirect,
      modifyResponse,
      method,
      ifTop,
      getRequest,
      getParam,
      setContentType,
      runSnaplet,
      nestSnaplet,
      makeSnaplet,
      combineConfig,
      addRoutes )
import Snap.Util.FileServe ( serveDirectory )
import Text.Blaze.Html.Renderer.Text ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Database.Stronghold as S
    ( Change(Change),
      Version,
      parent,
      listToPath,
      bsToVersion,
      versionToText,
      versionToBS,
      newClient,
      headRef,
      at,
      before,
      after,
      peculiar,
      materialized,
      paths,
      fetchVersionInfo,
      updatePath )
import           Network.HTTP.Conduit
import qualified Network.OAuth.OAuth2 as OAuth2
    ( OAuth2(OAuth2),
      ExchangeToken(ExchangeToken),
      OAuth2Token(accessToken),
      authorizationUrl )
import Network.OAuth.OAuth2.HttpClient ( fetchAccessToken )
import Github ( GithubUser(githubLogin), userInfo )
import URI.ByteString ( Scheme(Scheme), Host(Host), serializeURIRef' )
import System.IO ( Handle, stdout, stderr )
import Types
    ( StrongholdApp(StrongholdApp, _storedHead, _stronghold),
      AppConfig(AppConfig, portNum),
      sess,
      storedHead )
import URI ( mkURI )
import Views
    ( nodeTemplate, versionTemplate, diffTemplate, constructTree, renderTree )
import JsonDiff
    ( diffJson )


appInit :: AppConfig -> Manager -> SnapletInit StrongholdApp StrongholdApp
appInit (AppConfig strongholdURL githubKeys authorised _ sessionSecretPath assetsPath) manager = do
  makeSnaplet "stronghold" "The management UI for stronghold" Nothing $ do
    session <- nestSnaplet "" sess $
      initCookieSessionManager sessionSecretPath "sess" Nothing Nothing
    addRoutes [
      ("/", home),
      ("/at", at),
      ("/login", login),
      ("/auth/github/callback", githubCallback),
      ("/logout", logout),
      ("/:version/info", info),
      ("/:version/node", node),
      ("/:version/update", updateNode),
      ("/assets", serveDirectory assetsPath)
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
  login = Snap.method GET $ ifTop $ redirect $ serializeURIRef' $ OAuth2.authorizationUrl githubKeys

  githubCallback :: Handler StrongholdApp StrongholdApp ()
  githubCallback = Snap.method GET $ ifTop $ do
    code <- getParam "code"
    case code of
      Nothing -> empty
      Just code' -> do
        token <- liftIO $ fetchAccessToken manager githubKeys $ OAuth2.ExchangeToken $ decodeUtf8 code'
        case token of
          Left s -> liftIO $ print s
          Right token' -> do
            user <- liftIO $ userInfo manager (OAuth2.accessToken token')
            case user of
              Nothing -> writeText "no user"
              Just user' ->
                if isAuthorized authorised user' then do
                  with sess $ do
                    setInSession "author" (githubLogin user')
                    commitSession
                  redirect "/"
                else
                  writeText (Text.concat ["not authorised ", githubLogin user'])

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
  assetsPath <- require config "assets-path"
  let
    authorizeURI = mkURI (Scheme "https") (Host "github.com") "/login/oauth/authorize"
    tokenURI = mkURI (Scheme "https") (Host "github.com") "/login/oauth/access_token"
  return
    (AppConfig
      strongholdURL
      (OAuth2.OAuth2
        clientID
        clientSecret
        authorizeURI
        tokenURI
        Nothing)
      authorised'
      portNum
      sessionSecretPath
      assetsPath)

isAuthorized :: [Text] -> GithubUser -> Bool
isAuthorized authorised user = elem (githubLogin user) authorised

writeTo :: Handle -> ConfigLog
writeTo handle = ConfigIoLog (BC.hPutStrLn handle)

applyAll :: [a -> a] -> a -> a
applyAll = appEndo . mconcat . map Endo

serveSnaplet' :: Config Snap AppConfig -> SnapletInit b b -> IO ExitCode
serveSnaplet' config initializer = do
  (msgs, handler, doCleanup) <- runSnaplet Nothing initializer

  (conf, site) <- combineConfig config handler
  let serve = simpleHttpServe conf

  liftIO $ TIO.hPutStrLn stdout msgs
  v <- try $ serve $ site :: IO (Either SomeException ())
  exitCode <- case v of
    Left exception -> (print exception) >> (return $ ExitFailure 1)
    Right () -> return ExitSuccess
  doCleanup
  return exitCode

main :: IO ()
main = do
  [configFile] <- getArgs
  config <- fetchConfig configFile
  let snapConfig =
        applyAll [
          setPort (portNum config),
          setAccessLog (writeTo stdout),
          setErrorLog (writeTo stderr)
        ] defaultConfig
  manager <- newManager tlsManagerSettings
  serveSnaplet' snapConfig (appInit config manager) >>= exitWith
