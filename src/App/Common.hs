module App.Common where

import Control.Monad
import Data.Maybe hiding (catMaybes)
import Data.Text as T
import Prelude as P
import Reflex.Dom

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

toText :: Show a => a -> Text
toText = T.pack . show

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _ = Nothing
    go  (x:_) 0 = Just x
    go  (_:xs) i = go xs (pred i)

