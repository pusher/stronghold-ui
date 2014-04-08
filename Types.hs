{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types where

import Control.Lens.TH ( makeLenses )
import Snap.Snaplet.Session ( SessionManager )
import Snap ( Snaplet )
import qualified Database.Stronghold as S
    ( Change, MetaInfo, Version, Client, Path )
import qualified Network.OAuth.OAuth2 as OAuth2 ( OAuth2 )
import Github ( GithubUser )


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
