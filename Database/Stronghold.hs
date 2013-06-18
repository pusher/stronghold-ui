{-# LANGUAGE OverloadedStrings #-}
module Database.Stronghold (
  Path,
  parent,
  textToPath,
  pathToText,
  pathToList,
  listToPath,
  viewl,
  singletonPath,
  Client,
  Version,
  bsToVersion,
  textToVersion,
  versionToText,
  versionToBS,
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
import Data.Aeson (fromJSON, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.HashMap.Strict as HashMap
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM)

import qualified Network.HTTP as HTTP
import Network.URI (URI, relativeTo, parseRelativeReference, parseURI)

type JSON = Aeson.Value

utcFromInteger :: Integer -> UTCTime
utcFromInteger = posixSecondsToUTCTime . fromIntegral

newtype Path = Path [Text] deriving Eq

instance Show Path where
  show path = Text.unpack $ Text.concat ["Path ", pathToText path]

parent :: Path -> Maybe Path
parent (Path []) = Nothing
parent (Path xs) = (Just . Path . reverse . drop 1 . reverse) xs

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

pathToList :: Path -> [Text]
pathToList (Path p) = p

viewl :: Path -> Maybe (Text, Path)
viewl (Path []) = Nothing
viewl (Path (x:xs)) = Just (x, Path xs)

singletonPath :: Text -> Path
singletonPath = Path . return

instance Monoid Path where
  mempty = Path []
  mappend (Path x) (Path y) = Path (x ++ y)

data Client = Client URI
newtype Version = Version Text
data MetaInfo = MetaInfo UTCTime Text Text -- timestamp, comment, author
data Change = Change Path JSON JSON

-- not sure this is a good idea
textToVersion :: Text -> Version
textToVersion = Version

bsToVersion :: B.ByteString -> Version
bsToVersion = textToVersion . decodeUtf8

versionToText :: Version -> Text
versionToText (Version v) = v

versionToBS :: Version -> B.ByteString
versionToBS = encodeUtf8 . versionToText

instance Aeson.FromJSON Change where
  parseJSON = undefined

query :: HTTP.HStream x => Client -> URI -> IO x
query (Client baseURI) path = do
  let uri = path `relativeTo` baseURI
  rsp <- HTTP.simpleHTTP (HTTP.mkRequest HTTP.GET uri)
  code <- HTTP.getResponseCode rsp
  case code of
    (2, 0, 0) -> HTTP.getResponseBody rsp
    code -> fail ("bad status: " ++ show code)

postJSON :: Client -> URI -> JSON -> IO (Either Text B.ByteString)
postJSON (Client baseURI) path body = do
  let uri = path `relativeTo` baseURI
  let req = HTTP.mkRequest HTTP.POST uri :: HTTP.Request B.ByteString
  let body' = (B.concat . BL.toChunks . Aeson.encode) body
  let req' = setBody body' req
  rsp <- HTTP.simpleHTTP req'
  code <- HTTP.getResponseCode rsp
  body <- HTTP.getResponseBody rsp
  case code of
    (2, 0, 0) ->
      return (Right body)
    code ->
      return $ Left $ Text.concat ["http post failed: ", Text.pack $ show code, " ", decodeUtf8 body]
 where
  setBody :: B.ByteString -> HTTP.Request B.ByteString -> HTTP.Request B.ByteString
  setBody body req =
    HTTP.replaceHeader HTTP.HdrContentType "application/json" $
    HTTP.replaceHeader HTTP.HdrContentLength (show $ B.length body) $
    req {HTTP.rqBody = body}

queryJSON :: Aeson.FromJSON a => Client -> URI -> IO a
queryJSON client path =
  fmap Aeson.decode (query client path) >>= maybe (fail "bad json") return

textToURI :: Text -> URI
textToURI = fromJust . parseRelativeReference . Text.unpack

constructURI :: Text -> [(Text, Text)] -> URI
constructURI path qs =
  let qs' = concatMap (\(k, v) -> [k, "=", v]) qs
      qs'' = case qs' of
              [] -> []
              _ -> ("?" : qs') in
    (textToURI . Text.concat . (path :)) qs''

resultToM :: Monad m => Aeson.Result a -> m a
resultToM (Aeson.Success x) = return x
resultToM _ = fail "incorrect json structure"

newClient :: String -> IO Client
newClient =
  fmap Client . maybe (fail "couldn't parse url") return . parseURI

headRef :: Client -> IO Version
headRef client = (Version . decodeUtf8) <$> query client ((textToURI . Text.concat) ["/head"])

at :: Client -> UTCTime -> IO Version
at client ts = do
  let uri = constructURI "/versions" [("at", Text.pack (show 0))]
  (Version . decodeUtf8) <$> query client uri

(.>) :: Aeson.FromJSON a => Text -> JSON -> Maybe a
key <.> (Aeson.Object obj) = HashMap.lookup key obj
_ .> _ = Nothing

before :: Client -> Version -> Maybe Int -> IO [(Version, MetaInfo)]
before client (Version version) limit = do
  let qs = [("last", version)] ++ maybe [] (\n -> [("size", Text.pack $ show n)]) limit
  let uri = constructURI "/versions" qs
  result <- queryJSON client uri
  maybe (fail "incorrect json structure") return (structureChanges result)
 where
  structureChanges :: JSON -> Maybe [(Version, MetaInfo)]
  structureChanges dat = do
    dat' <- resultToM $ fromJSON dat
    forM dat' (\x ->
      (,) <$>
        (Version <$> ("version" .> x)) <*>
        (MetaInfo <$>
          (utcFromInteger <$> ("timestamp" .> x)) <*>
          ("comment" .> x) <*>
          ("author" .> x)))

peculiar :: Client -> Version -> Path -> IO JSON
peculiar client (Version version) path = do
  let uri = (textToURI . Text.concat) ["/", version, "/tree/peculiar", pathToText path]
  queryJSON client uri

materialized :: Client -> Version -> Path -> IO JSON
materialized client (Version version) path = do
  let uri = (textToURI . Text.concat) ["/", version, "/tree/materialized", pathToText path]
  queryJSON client uri

nextMaterialized :: Client -> Version -> Path -> IO (Version, JSON)
nextMaterialized client (Version version) path = do
  let uri = (textToURI . Text.concat) ["/", version, "/next/tree/materialized", pathToText path]
  result <- queryJSON client uri
  let result' = (,) <$> (Version <$> ("version" .> result)) <*> ("data" .> result)
  maybe (fail "incorrect json structure") return result'

paths :: Client -> Version -> IO [Path]
paths client (Version version) = do
  let uri = (textToURI . Text.concat) ["/", version, "/tree/paths"]
  result <- queryJSON client uri
  maybe (fail "incorrect json structure") return (structurePaths result)
 where
  structurePaths :: JSON -> Maybe [Path]
  structurePaths dat = (resultToM . fromJSON) dat >>= mapM textToPath

info :: Client -> Version -> IO (MetaInfo, [Change])
info client (Version version) = do
  let uri = (textToURI . Text.concat) ["/", version, "/change"]
  result <- queryJSON client uri
  maybe (fail "incorrect json structure") return (structureChange result)
 where
  structureChange :: JSON -> Maybe (MetaInfo, [Change])
  structureChange dat =
    (,) <$>
      (MetaInfo <$>
        (utcFromInteger <$> ("timestamp" .> dat)) <*>
        ("comment" .> dat) <*>
        ("author" .> dat)) <*>
      ("changes" .> dat)

updatePath :: Client -> Version -> Path -> JSON -> Text -> Text -> IO (Either Text Version)
updatePath client (Version version) path json author comment = do
  let uri = (textToURI . Text.concat) ["/", version, "/update", pathToText path]
  let dat = Aeson.object [("author", toJSON author), ("comment", toJSON comment), ("data", json)]
  result <- postJSON client uri dat
  return (fmap (Version . decodeUtf8) result)
