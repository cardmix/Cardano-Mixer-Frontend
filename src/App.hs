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

data FormState = Deposit | Withdraw deriving (Eq, Show)

appWidget :: MonadWidget t m => m ()
appWidget = divClass "sectionapplication wf-section" .
  divClass "maincontainer w-container" . divClass "columns-3 w-row" $ do
    mainForm
    readme
  where
    readme = divClass columnClass $ do
      divClass "divblockmain" . elClass "h2" "headingsection headingsmall" $
        text "ReadMe"
      elClass "p" "maintext" $ text readmeText
    readmeText =
      "You can deposit «these» tokens and «these» amounts. \
      \To deposit you should first connect to the wallet. \
      \«These» wallets are accepted. When deposit don’t forget to save \
      \secret key since you need it to withdraw. Wait a bit before withdrawing \
      \to improve privacy and mine some ada. To withdraw provide a secret key \
      \corresponding to a deposit and also an wallet address. You can autofil \
      \the withdraw address by connecting to the wallet."

columnClass :: Text
columnClass = "w-col w-col-6 w-col-medium-6 w-col-small-small-stack"

mainForm :: MonadWidget t m => m ()
mainForm = divClass columnClass . divClass "divblockcentered" .
  divClass "columns-4 w-row" $ mdo
    dFormState <- holdDyn Deposit $ leftmost [eDeposit, eWithdraw]
    isEnabled <- Nami.isEnabled
    dWalletConnected <- holdDyn isEnabled (True <$ eConnect) -- make this an input to pass values
    eDeposit <- buttonSwitch Deposit dFormState
    eConnect <- divClass colCls8 $ do
      eeConnect <- dyn $ dFormState <&> \case
        Deposit -> depositForm dWalletConnected
        Withdraw -> withdrawForm >> return never
      switchHold never eeConnect
    performEvent_ (Nami.enable <$ eConnect)
    eWithdraw <- buttonSwitch Withdraw dFormState
    blank
  where
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
