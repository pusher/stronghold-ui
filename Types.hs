{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types where

import Data.Maybe ()
import Data.Monoid ()
import Data.Text ()
import qualified Data.Text as Text ()
import qualified Data.Text.IO as TIO ()
import Data.Text.Encoding ()
import qualified Data.ByteString as B ()
import qualified Data.ByteString.Char8 as BC ()
import qualified Data.ByteString.Lazy as BL ()
import qualified Data.Aeson as Aeson ()
import Data.Aeson ()
import Data.Tree ()
import Data.HashMap.Strict ()
import qualified Data.HashMap.Strict as HashMap ()
import Data.List ()
import Data.Time.Clock ()
import Data.Configurator ()
import Data.Configurator.Types ()
import Control.Applicative ()
import Control.Lens ()
import Control.Lens.TH ( makeLenses )
import Control.Monad ()
import Control.Exception ()
import System.Environment ()
import System.Exit ()
import Snap.Snaplet.Session ( SessionManager )
import Snap.Snaplet.Session.Backends.CookieSession ()
import Snap ( Snaplet )
import Snap.Util.FileServe ()
import Text.Blaze.Html.Renderer.Text ()
import Text.Blaze.Html5 ()
import qualified Text.Blaze.Html5 as H ()
import qualified Text.Blaze.Html5.Attributes as A ()
import qualified Database.Stronghold as S
    ( Change, MetaInfo, Version, Client, Path )
import qualified Network.OAuth.OAuth2 as OAuth2 ( OAuth2 )
import Network.OAuth.OAuth2.HttpClient ()
import Github ( GithubUser )
import System.IO ()


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
  sessionSecretPath :: FilePath,
  assetsPath :: FilePath
}


type VersionsInfo = [(S.Version, S.MetaInfo, [S.Path])]
type UpdateInfo = ((S.Version, S.MetaInfo, [S.Path]), [S.Change])
