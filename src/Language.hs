module Language where

import           Control.Monad.Extra     (mconcatMapM)
import           Data.Text               (Text)
import           Reflex.Dom

import qualified JS


data WebsiteTextEntry = WebsiteTextEntry
  {
    elementId :: Text,
    valueEn :: Text,
    valueRu :: Text
  }

testTextEntrys :: [WebsiteTextEntry]
testTextEntrys = [WebsiteTextEntry "testElement" "Test value" "Тестовое значение"]

switchLanguage :: MonadWidget t m => Event t (WebsiteTextEntry -> Text) -> m ()
switchLanguage e = mconcatMapM (\wte -> performEvent_ (JS.setElementText (elementId wte) <$> fmap ($ wte) e)) testTextEntrys

languageSwitcher :: MonadWidget t m => m ()
languageSwitcher = divClass "divblockcentered" $ mdo
  blank

languageItem :: MonadWidget t m => m (Event t ())
languageItem = do
    (e, _) <- el' "div" (text "EN")
    return $ () <$ domEvent Click e