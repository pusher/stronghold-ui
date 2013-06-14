{-# LANGUAGE OverloadedStrings #-}
module Database.Stronghold (
  Path,
  textToPath,
  listToPath,
  Client,
  Version,
  newClient,
  headRef,
  at,
  before,
  materialized,
  peculiar,
  nextMaterialized,
  info,
  paths,
  updatePath
) where

import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock (UTCTime)

import Control.Applicative ((<$>))
import Control.Monad (forM)

import qualified Network.HTTP as HTTP
import Network.URI (URI, relativeTo, parseRelativeReference, parseURI)

type JSON = Aeson.Value

newtype Path = Path [Text]

textToPath :: Text -> Maybe Path
textToPath t =
  if Text.null t then
    return mempty
  else do
    t' <- Text.stripPrefix "/" t
    if Text.last t' == '/' then
      Nothing
     else
      (return . Path . Text.splitOn "/") t'

listToPath :: [Text] -> Path
listToPath = Path

pathToText :: Path -> Text
pathToText (Path p) = Text.concat (concatMap (\x -> ["/", x]) p)

instance Monoid Path where
  mempty = Path []
  mappend (Path x) (Path y) = Path (x ++ y)

data Client = Client URI
newtype Version = Version Text
data MetaInfo = MetaInfo UTCTime Text Text -- timestamp, comment, author
data Change = Change Path JSON JSON

instance Aeson.FromJSON Change where
  parseJSON = undefined

query :: Client -> URI -> IO BL.ByteString
query (Client baseURI) path = do
  let uri = path `relativeTo` baseURI
  rsp <- HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET uri)
  code <- HTTP.getResponseCode rsp
  case code of
    (2, 0, 0) -> HTTP.getResponseBody rsp
    _ -> fail "bad status"

queryByteString :: Client -> URI -> IO B.ByteString
queryByteString client path = (B.concat . BL.toChunks) <$> query client path

queryJSON :: Aeson.FromJSON a => Client -> URI -> IO a
queryJSON client path =
  fmap Aeson.decode (query client path) >>= maybe (fail "bad json") return

plah :: Text -> [(Text, Text)] -> URI
plah path qs =
  let qs' = concatMap (\(k, v) -> [k, "=", v]) qs
      qs'' = case qs' of
              [] -> []
              _ -> ("?" : qs') in
    (plah1 . (path :)) qs''

plah1 :: [Text] -> URI
plah1 = fromJust . parseRelativeReference . Text.unpack . Text.concat

plah6 :: Maybe a -> Aeson.Result a
plah6 Nothing = Aeson.Error ""
plah6 (Just x) = Aeson.Success x

plah2 :: JSON -> Aeson.Result [(Version, MetaInfo)]
plah2 dat = do
  dat' <- Aeson.fromJSON dat :: Aeson.Result [HashMap.HashMap Text JSON]
  forM dat' (\x -> do
    version <- plah6 $ HashMap.lookup "version" x
    version' <- Version <$> Aeson.fromJSON version
    comment <- plah6 $ HashMap.lookup "comment" x
    comment' <- Aeson.fromJSON comment
    author <- plah6 $ HashMap.lookup "author" x
    author' <- Aeson.fromJSON author
    ts <- plah6 $ HashMap.lookup "timestamp" x
    ts' <- Aeson.fromJSON ts
    return (version', MetaInfo ts' comment' author'))

plah3 :: JSON -> Aeson.Result (Version, JSON)
plah3 dat = do
  dat' <- Aeson.fromJSON dat :: Aeson.Result (HashMap.HashMap Text JSON)
  version <- plah6 $ HashMap.lookup "version" dat'
  version' <- Version <$> Aeson.fromJSON version
  dat'' <- plah6 $ HashMap.lookup "data" dat'
  return (version', dat'')

plah4 :: JSON -> Aeson.Result [Path]
plah4 dat = do
  dat' <- Aeson.fromJSON dat :: Aeson.Result [Text]
  mapM (plah6 . textToPath) dat'

plah5 :: JSON -> Aeson.Result (MetaInfo, [Change])
plah5 dat = do
  dat' <- Aeson.fromJSON dat :: Aeson.Result (HashMap.HashMap Text JSON)
  comment <- plah6 $ HashMap.lookup "comment" dat'
  comment' <- Aeson.fromJSON comment
  author <- plah6 $ HashMap.lookup "author" dat'
  author' <- Aeson.fromJSON author
  ts <- plah6 $ HashMap.lookup "timestamp" dat'
  ts' <- Aeson.fromJSON ts
  changes <- plah6 $ HashMap.lookup "changes" dat'
  changes' <- Aeson.fromJSON changes
  return (MetaInfo ts' comment' author', changes')

resultToM :: Monad m => Aeson.Result a -> m a
resultToM (Aeson.Success x) = return x
resultToM _ = fail "incorrect json structure"

newClient :: String -> IO (Maybe Client)
newClient =
  return . fmap Client . parseURI

headRef :: Client -> IO Version
headRef client = (Version . decodeUtf8) <$> queryByteString client (plah1 ["/head"])

at :: Client -> UTCTime -> IO Version
at client ts = do
  let uri = plah "/versions" [("at", Text.pack (show 0))]
  (Version . decodeUtf8) <$> queryByteString client uri

before :: Client -> Version -> Maybe Int -> IO [(Version, MetaInfo)]
before client (Version version) limit = do
  let qs = [("last", version)] ++ maybe [] (\n -> [("size", Text.pack $ show n)]) limit
  let uri = plah "/versions" qs
  result <- queryJSON client uri
  resultToM (plah2 result)

peculiar :: Client -> Version -> Path -> IO JSON
peculiar client (Version version) path = do
  let uri = plah1 ["/", version, "/tree/peculiar", pathToText path]
  queryJSON client uri

materialized :: Client -> Version -> Path -> IO JSON
materialized client (Version version) path = do
  let uri = plah1 ["/", version, "/tree/materialized", pathToText path]
  queryJSON client uri

nextMaterialized :: Client -> Version -> Path -> IO (Version, JSON)
nextMaterialized client (Version version) path = do
  let uri = plah1 ["/", version, "/next/tree/materialized", pathToText path]
  result <- queryJSON client uri
  resultToM (plah3 result)

paths :: Client -> Version -> IO [Path]
paths client (Version version) = do
  let uri = plah1 ["/", version, "/tree/paths"]
  result <- queryJSON client uri
  resultToM (plah4 result)

info :: Client -> Version -> IO (MetaInfo, [Change])
info client (Version version) = do
  let uri = plah1 ["/", version, "/change"]
  result <- queryJSON client uri
  resultToM (plah5 result)

updatePath :: Client -> Version -> Path -> JSON -> IO (Maybe Version)
updatePath client version path json = undefined
