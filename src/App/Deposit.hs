module App.Deposit where

import App.Common
import Backend
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Functor ((<&>))
import Data.List as L
import Data.Maybe hiding (catMaybes)
import Data.Text as T
import Data.Witherable (catMaybes)
import JS
import Prelude as P
import Reflex.Dom
import Text.Read (readMaybe)

data ButtonState = ButtonConnect | ButtonDeposit deriving Eq

depositForm :: MonadWidget t m => Dynamic t Bool -> m (Event t ())
depositForm dWalletConnected = do
  eConn <- divClass "appformwrapper". divClass "form-block w-form"
    . el "form" $ mdo
      eToken <- selectInput "Token" tokenHint showToken TokenADA [minBound..]
      dToken <- holdDyn initToken eToken
      eAmount <- switchHold never =<< dyn (dToken <&> \tok -> do
        let
          range = tokenToAmountRange tok
          initAmount = tokenInitAmount tok
        selectInput "Amount" amountHint toText initAmount range)
      dAmount <- holdDyn (tokenInitAmount initToken) $ leftmost
        [eAmount, tokenInitAmount <$> eToken]
      inputTitle "Secret Key" keyHint
      key <- secretKeyInput (() <$ eDeposit)
      eeBtn <- divClass "mainbuttonwrapper" $ dyn $ dWalletConnected <&> \case
        False -> do
          (e, _) <- elAttr' "a" ("class" =: "button w-button") $
            text "Connect to wallet"
          return (ButtonConnect <$ domEvent Click e)
        True -> do
          (e, _) <- elAttr' "a" ("class" =: "button w-button") $
            text "Deposit"
          return (ButtonDeposit <$ domEvent Click e)
      eBtn <- switchHold never eeBtn
      let
        eConnect = ffilter (== ButtonConnect) eBtn
        eDeposit = ffilter (== ButtonDeposit) eBtn
        depositArgs = zipDyn dToken dAmount
      runDeposit elId key depositArgs (() <$ eDeposit)
      return $ () <$ eConnect
  elAttr "div" ("class" =: "text-block-2" <> "id" =: elId) blank
  return eConn
  where
    elId = "deposit-form-msg"
    initToken = TokenADA
    tokenInitAmount tok = let range = tokenToAmountRange tok in
      bool (P.head range) 0 (P.null range)
    showToken = \case
      TokenADA -> "tADA"
      TokenMIX -> "tMIX"
    tokenHint = "Choose a Cardano Blockchain native asset for a deposit. Non-ADA deposits require some ADA to cover the fees."
    amountHint = "Choose the amount of tokens to deposit."
    keyHint = pack $ "Save the secret key generated during a deposit. It will be used to withdraw tokens from the protocol. " ++ 
      "A copy of the Secret Key is automatically saved in your default Downloads folder."

secretKeyInput :: MonadWidget t m => Event t () -> m Integer
secretKeyInput eDeposit = divClass "secretekeywrapper" . divClass "w-row" $ do
  eKey <- performEvent (liftIO generateDepositSecret <$ eDeposit)
  dKey <- holdDyn (DepositSecret (Zp 0) (Zp 0)) eKey
  performEvent_ (fmap (JS.saveTextFile . toText) eKey)
  let keyId = "TextSecretKey"
  divClass colCls11 $ do
    let
      keyEnabledAttrs = "class" =: "textform" <> "id" =: keyId <> style
      keyDisabledAttrs = "class" =: "textform unable" <> style
      style = "style" =: "width: 300.66px; word-wrap: inherit; \
        \white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
    keyInpAttrs <- holdDyn keyDisabledAttrs (keyEnabledAttrs <$ eDeposit)
    keyInpText <- holdDyn "Please, save the secret key!" (toText key <$ eDeposit)
    elDynAttr "div" keyInpAttrs $ dynText keyInpText
  divClass colCls4 $ do
    let
      copyEnabledAttrs = "class" =: "buttoncopy w-inline-block"
        <> "style" =: "cursor:pointer;"
      copyDisabledAttrs = "class" =: "buttoncopy unable w-inline-block"
    copyBtnAttrs <- holdDyn copyDisabledAttrs (copyEnabledAttrs <$ eDeposit)
    (e, _) <- elDynAttr' "a" copyBtnAttrs $ elAttr "img"
      ("src" =: "/images/PictCopy.svg" <> "loading" =: "lazy" <>
        "width" =: "23" <> "class" =: "pictcopy") blank
    performEvent_ (copyElemContent keyId <$ domEvent Click e)
  return key
  where
    colCls11 = "w-col w-col-11 w-col-small-11 w-col-tiny-11"
    colCls4 = "column-4 w-col w-col-1 w-col-small-1 w-col-tiny-1"

selectInput :: (MonadWidget t m, Eq a) => Text -> Text -> (a -> Text) -> a
  -> [a] -> m (Event t a)
selectInput title hint showFunc initVal valsRange = do
  inputTitle title hint
  input <- fmap (_selectElement_change . fst) $ selectElement (def
    & initialAttributes .~ ("class" =: "select-field w-select")
    & selectElementConfig_initialValue .~ initValIdx)
    $ do
      mapM_ mkOption . P.zip [0..] $ valsRange
  return $ catMaybes $ parseVal <$> input
  where
    mkOption (idx::Int, val) = elAttr "option" ("value" =: toText idx) . text .
      showFunc $ val
    parseVal txt = readMaybe @Int (T.unpack txt) >>= safeIndex valsRange
    initValIdx = maybe "-1" toText $ L.elemIndex initVal valsRange
