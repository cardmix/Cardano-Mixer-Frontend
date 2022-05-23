module App where

import App.Common
import App.Deposit
import App.Withdraw
import Control.Monad
import Data.Bool
import Data.Functor ((<&>))
import Data.List as L
import Data.Text as T
import NamiJS as Nami
import Prelude as P
import Reflex.Dom

import Reflex.Dom.Contrib.Widgets.ScriptDependent

data FormState = Deposit | Withdraw deriving (Eq, Show)

appWidget :: MonadWidget t m => m ()
appWidget = divClass "sectionapplication wf-section" .
  divClass "maincontainer w-container" . divClass "columns-2 w-row" $ do
    mainForm
    readme
  where
    readme = divClass columnClass $ do
      elClass "h6" "headingsection headingsmall" $
        text "ReadMe"
      elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $ do
        text "See this "
        elAttr "a" ("href" =: "https://cardmix.medium.com/public-test-1-details-efbf4e3264cb" <> "class" =: "link" 
          <> "target" =: "_blank") $ text "Medium"
        text " post for a complete Public Test guide."
      elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $ do
        text "To get some tADA, use the following "
        elAttr "a" ("href" =: "https://testnets.cardano.org/en/testnets/cardano/tools/faucet/" <> "class" =: "link" 
          <> "target" =: "_blank") $ text "link"
        text ". To get tMIX, send 2 tADA to the following address: "
        divClass "divcalculatorwrapper" . elAttr "strong" ("class" =: "bold-text-5") $ text
          "addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk\
          \5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw"
        text "This version works only with " 
        elAttr "a" ("href" =: "https://namiwallet.io/" <> "class" =: "link" 
          <> "target" =: "_blank") $ text "Nami"
        text " wallet."
        -- , but we will support more wallets in the future."
      elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $ do
        text
          "The deposit Secret Key is automatically saved into your default Downloads folder. \
          \ You could give the Secret Key to someone else or use the Key yourself (e.g., with another wallet). \
          \ Waiting before withdrawing improves your privacy."
      elAttr "p" ("class" =: "parwide" <> "align" =: "justify") $ do
        text
          "Please, fill out the "
        elAttr "a" ("href" =: "https://forms.gle/dfXjh4DqY16mKz4X7" <> "class" =: "link" 
          <> "target" =: "_blank") $ text "feedback form"
        text 
          " to help us improve the app. \
          \ If you are an ISPO participant, you can earn some extra MIX tokens by doing this."
        

      -- "This test version allows sending tADA and tMIX privately on the Cardano Testnet. \
      -- \ To get tADA, use the following link https://testnets.cardano.org/en/testnets/cardano/tools/faucet/. \
      -- \ To get tMIX, send 2 tADA to the following address \
      -- \ addr_test1qrh8caw4kmlkwydwzdehpyw905dg8ayjv0vpe6vqmkkk5q3psddwydp9ea0gj3jawxyak3d238jpj9fxx3gnfhk7paxqnw2xmw . \
      

columnClass :: Text
columnClass = "w-col w-col-6 w-col-medium-6 w-col-small-small-stack"

mainForm :: MonadWidget t m => m ()
mainForm = divClass columnClass . divClass "divblockcentered" .
  divClass "columns-4 w-row" $ mdo
    dFormState <- holdDyn Deposit $ leftmost [eDeposit, eWithdraw]
    ePb <- getPostBuild
    eNamiLoaded <- updated <$> widgetHoldUntilDefined "namiIsEnabled" ("js/Nami.js" <$ ePb) blank blank
    performEvent_ (Nami.isEnabled elemId <$ eNamiLoaded)
    dWalletConnected <- fmap (fmap parseConnected . value) $ inputElement $ def
      & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId)
      & inputElementConfig_initialValue .~ "false"
    eDeposit <- buttonSwitch Deposit dFormState
    eConnect <- divClass colCls8 $ do
      eeConnect <- dyn $ dFormState <&> \case
        Deposit -> depositForm dWalletConnected
        Withdraw -> withdrawForm >> return never
      switchHold never eeConnect
    performEvent_ (Nami.enable elemId <$ eConnect)    
    eWithdraw <- buttonSwitch Withdraw dFormState
    blank
  where
    parseConnected "true" = True
    parseConnected _ = False
    elemId = "input-is-enabled"
    colCls8 = "w-col w-col-8 w-col-medium-8 w-col-small-8 w-col-tiny-8"

buttonSwitch :: MonadWidget t m =>
  FormState -> Dynamic t FormState -> m (Event t FormState)
buttonSwitch s dCur = divClass cls $ do
  (e, _) <- elDynAttr' "a" (mkAttrs <$> dCur) .
    sequence_ . L.intersperse br $ (text . T.singleton) <$> (show s)
  return $ s <$ domEvent Click e
  where
    cls = "w-col w-col-2 w-col-medium-2 w-col-small-2 w-col-tiny-2"
    br = el "br" blank
    mkAttrs cur = "class" =: T.unwords
      ["button" <> T.toLower (toText s), "w-button", bool "" "press" (cur == s)]
