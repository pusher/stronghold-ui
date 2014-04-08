module JsonDiff where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Text (Text)
import qualified Data.Vector as Vector

type KeyDiff = (Text, JsonDiff)
type PosDiff = (Int, JsonDiff)

data JsonDiff =
  Same |
  HashChange [KeyDiff] |
  ArrayChange [PosDiff] |
  Replace Aeson.Value Aeson.Value |
  Added Aeson.Value |
  Removed Aeson.Value

-- Change of value
-- Replace
-- Addition
-- Removal

diffJson :: Aeson.Value -> Aeson.Value -> JsonDiff
diffJson a b =
  if a == b then
    Same
  else
    case (a, b) of
    (Aeson.Object a, Aeson.Object b) ->
      let
        ka = S.fromList $ H.keys a
        kb = S.fromList $ H.keys b
        keys = S.toList $ S.union ka kb
        diff = foldr (foldingThing a b) [] keys
      in
        HashChange diff
    (Aeson.Array a, Aeson.Array b) ->
      let
        lista = Vector.toList a
        listb = Vector.toList b
        z = extendingZip lista listb
        y = zip z (iterate (+1) 0)
        diff = foldr (uncurry arrayFoldingThing) [] y
      in
        ArrayChange diff
    _ ->
      Replace a b


diffCase :: Maybe Aeson.Value -> Maybe Aeson.Value -> JsonDiff
diffCase (Just a) (Just b) =
  diffJson a b
diffCase (Just a) Nothing =
  Removed a
diffCase Nothing (Just b) =
  Added b
diffCase Nothing Nothing = error "BUG"


foldingThing :: Aeson.Object -> Aeson.Object -> Text -> [KeyDiff] -> [KeyDiff]
foldingThing a b key =
  let
    aval = H.lookup key a
    bval = H.lookup key b
  in
    if aval == bval then
      id
    else
      (:) (key, diffCase aval bval)

arrayFoldingThing :: (Maybe Aeson.Value, Maybe Aeson.Value) -> Int -> [PosDiff] -> [PosDiff]
arrayFoldingThing (a,b) pos =
  (:) (pos, diffCase a b)

extendingZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
extendingZip [] [] = []
extendingZip (a:as) [] =
  (Just a, Nothing) : extendingZip as []
extendingZip [] (b:bs) =
  (Nothing, Just b) : extendingZip [] bs
extendingZip (a:as) (b:bs) =
  (Just a, Just b) : extendingZip as bs


