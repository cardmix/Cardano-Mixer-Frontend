module WebPage.Basic where

import           Data.Bool              (bool)
import           Data.List              (elemIndex)
import           Data.Map               (Map)
import           Data.Text              (Text, unpack)
import           Reflex.Dom
import           Text.Read              (readMaybe)
import           Witherable             (catMaybes)

import           App.Common

-- Element containing the result of a JavaScript computation
elementResultJS :: MonadWidget t m => Text -> (Text -> a) -> m (Dynamic t a)
elementResultJS resId f = fmap (fmap f . value) $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: resId

-- Title of the input element along with a hint about the expected input
inputTitle :: MonadWidget t m => Text -> Text -> m ()
inputTitle title hint = divClass "w-row" $ do
  divClass colCls6 . elAttr "label" ("class" =: "field-label") $ text title
  divClass colCls3 . elAttr "a" ("class" =: "buttoninfo w-inline-block" <>
    "title" =: hint <> "style" =: "cursor:pointer;") .
    elAttr "img" ("src" =: "images/InfoPict.svg" <> "loading" =: "lazy" <>
      "width" =: "23" <> "class" =: "infopict") $ blank
  where
    colCls6 = "w-col w-col-6 w-col-medium-6 w-col-small-6 w-col-tiny-6"
    colCls3 = "column-3 w-col w-col-6 w-col-medium-6 w-col-small-6 w-col-tiny-6"

-- Dropdown list element
selectInput :: (MonadWidget t m, Eq a) => Text -> Text -> (a -> Text) -> a
  -> [a] -> m (Event t a)
selectInput title hint showFunc initVal valsRange = do
  inputTitle title hint
  input <- fmap (_selectElement_change . fst) $ selectElement (def
    & initialAttributes .~ ("class" =: "select-field w-select")
    & selectElementConfig_initialValue .~ initValIdx)
    $ do
      mapM_ mkOption . zip [0..] $ valsRange
  return $ catMaybes $ parseVal <$> input
  where
    mkOption (idx::Int, val) = elAttr "option" ("value" =: toText idx) . text .
      showFunc $ val
    parseVal txt = readMaybe @Int (unpack txt) >>= safeIndex valsRange
    initValIdx = maybe "-1" toText $ elemIndex initVal valsRange

appButton :: MonadWidget t m => Text -> Dynamic t Bool -> (a -> Map Text Text)-> Dynamic t a -> m (Event t ())
appButton title dVisible mkLinkAttrs linkState = do
  let mkVisible = bool ("style" =: "display: none;") ("class" =: "mainbuttonwrapper")
  (e, _) <- elDynAttr' "div" (mkVisible <$> dVisible) . elDynAttr' "a"
    (mkLinkAttrs <$> linkState) $ text title
  return (domEvent Click e)

appButtonAttrs :: Map Text Text
appButtonAttrs = "class" =: "button w-button" <> "style" =: "cursor:pointer;"

appButtonAttrsDisabled :: Map Text Text
appButtonAttrsDisabled = "class" =: "button w-button please-wait" <> "disabled" =: ""