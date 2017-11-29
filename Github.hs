{-# LANGUAGE OverloadedStrings #-}
module Github where

import Data.Text ( Text )
import Data.Aeson ( (.:) )
import qualified Data.Aeson as Aeson
    ( Value(Object), FromJSON(..) )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( mzero )
import qualified Network.OAuth.OAuth2 as OAuth2 ( OAuth2Result, AccessToken )
import Network.OAuth.OAuth2.HttpClient ( authGetJSON )
import           Network.HTTP.Conduit (Manager)
import URI.ByteString ( Scheme(Scheme), Host(Host) )
import URI ( mkURI )

data GithubUser = GithubUser {
  githubID :: Integer,
  githubLogin :: Text
} deriving Show

instance Aeson.FromJSON GithubUser where
  parseJSON (Aeson.Object o) =
    GithubUser <$> o .: "id" <*> o .: "login"
  parseJSON _ = mzero

-- TODO: Replace Maybe with Either String
userInfo :: Manager -> OAuth2.AccessToken -> IO (Maybe GithubUser)
userInfo manager token =
  let
    uri = mkURI (Scheme "https") (Host "api.github.com") "/user"
  in
    fmap (either (const Nothing) Just) ((authGetJSON manager token uri) :: IO (OAuth2.OAuth2Result () GithubUser))
