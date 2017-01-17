{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Views where

import Data.List ( foldl', intersperse, sortBy )
import Data.Ord ( comparing )
import Data.Maybe ( catMaybes )
import Data.Monoid ( Monoid(mappend, mempty) )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time.Clock ( UTCTime, diffUTCTime )
import Data.Tree ( Tree(Node) )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Database.Stronghold as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Snap ( when, forM_ )
import Text.Blaze.Html5 ( (!) )
import Types ( UpdateInfo, VersionsInfo )

import JsonDiff


navbar :: H.Html
navbar =
  H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
    H.div ! A.class_ "navbar-inner" $
      H.div ! A.class_ "container" $ do
        H.button !
          A.type_ "button" !
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
    H.script ! A.src "/assets/js/jquery.min.js" $ return ()
    H.script ! A.src "/assets/js/bootstrap.min.js" $ return ()
    H.script ! A.src "/assets/js/stable-stringify.js" $ return ()
    H.style "body { padding-top: 60px; }" -- Don't let the main content overlap the top bar
    headContent
  H.body $ do
    navbar
    H.div ! A.class_ "container" $ mainContent

nodeTemplate :: S.Version -> S.Path -> Aeson.Value -> Aeson.Value -> H.Html
nodeTemplate version path peculiar materialized =
  rootTemplate
    (do
      H.script ! A.src "/assets/jsoneditor/jsoneditor.min.js" $ return ()
      H.script ! A.src "/assets/js/node.js" $ return ()
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/jsoneditor/jsoneditor.min.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/css/node.css")
    (H.div ! A.class_ "row" $
      H.div ! A.class_ "span12" $ do
        H.div ! A.class_ "page-header" $ H.h4 $ H.toMarkup $ S.pathToText path
        H.p $
          H.div ! A.id "mode" ! A.class_ "btn-group" ! H.dataAttribute "toggle" "buttons-radio" $ do
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

diffTemplate :: JsonDiff -> H.Html
diffTemplate = jsonDiffToHtml

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

versionTemplate :: UTCTime -> S.Version -> H.Html -> Maybe (VersionsInfo, UpdateInfo) -> VersionsInfo -> H.Html
versionTemplate now version tree upto after =
  rootTemplate
    (do
      H.script ! A.src "/assets/js/version.js" $ return ()
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/css/hierarchy.css"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/assets/css/json_diff.css")
    (H.div ! A.class_ "row" $ do
      H.div ! A.class_ "span6" $ do
        H.h3 "Updates"
        when (not (null after)) $ do
          forM_ (reverse after) (renderShortVersion [])
          H.div ! A.id "newer-wrapper" $
            H.div ! A.id "newer" $ "newer ⇀"
        case upto of
          Just (before, (current, changes)) -> do
            (renderShortVersion changes) current ! A.id "current-change"
            when (not (null before)) $ do
              H.div ! A.id "older-wrapper-wrapper" $
                H.div ! A.id "older-wrapper" $
                  H.div ! A.id "older" $ "↼ older"
              forM_ before (renderShortVersion [])

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

  renderShortVersion :: [S.Change] -> (S.Version, S.MetaInfo, [S.Path]) -> H.Html
  renderShortVersion changes (version, S.MetaInfo ts comment author, paths) =
    H.div ! A.class_ "short-change" $ do
      makeTimeHTML now ts
      H.a ! (A.href $ H.toValue $ Text.concat ["/", S.versionToText version, "/info#current-change"]) $
        H.h5 $ H.toMarkup $ Text.concat [author, " updated ", renderPaths paths]
      H.toMarkup comment
      mapM_ changeToHtml changes

constructTree :: S.Path -> [S.Path] -> Tree S.Path
constructTree root =
  Node root . fmap f . groupByFirstLabel
 where
  f (label, subpaths) = constructTree (root `mappend` S.singletonPath label) subpaths

  groupByFirstLabel :: [S.Path] -> [(Text, [S.Path])]
  groupByFirstLabel =
    sortBy (comparing fst) .
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


changeToHtml :: S.Change -> H.Html
changeToHtml (S.Change _ before after) =
  jsonDiffToHtml $ diffJson before after

jsonDiffToHtml :: JsonDiff -> H.Html
jsonDiffToHtml =
  (H.pre ! A.class_ "json_diff") . diffToHtml

  where
    diffToHtml :: JsonDiff -> H.Html
    diffToHtml Same = "-"
    diffToHtml (HashChange keyDiffs) = do
      "{"
      renderCommas elems
      "}"
     where
      elems = map (uncurry kvToHtml) keyDiffs 
      kvToHtml :: Text -> JsonDiff -> H.Html
      kvToHtml key diff = do
        H.toMarkup key
        ": "
        diffToHtml diff

    diffToHtml (ArrayChange posDiffs) = do
      "["
      renderCommas elems
      "]"
     where
      elems = map (diffToHtml . snd) posDiffs
    diffToHtml (Replace old new) = do
      diffToHtml (Removed old)
      diffToHtml (Added new)
    diffToHtml (Added value) =
      H.ins $ jsonToHtml value
    diffToHtml (Removed value) =
      H.del $ jsonToHtml value

    jsonToHtml :: Aeson.Value -> H.Html
    jsonToHtml = H.unsafeLazyByteString . Aeson.encode

    renderCommas :: [H.Html] -> H.Html
    renderCommas = foldr (>>) (return ()) . intersperse ", "

