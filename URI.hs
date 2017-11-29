module URI where

import qualified Data.ByteString as B ( ByteString )
import URI.ByteString
    ( URIRef(..), Absolute, Scheme, Authority(Authority), Host, Query(..) )

mkURI :: Scheme -> Host -> B.ByteString -> URIRef Absolute
mkURI scheme host path =
  URI {
    uriScheme = scheme,
    uriAuthority = Just $ Authority Nothing host Nothing,
    uriPath = path,
    uriQuery = Query [],
    uriFragment = Nothing
  }
