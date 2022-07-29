module App.Deposit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bool              (bool)
import           Data.Functor           ((<&>))
import           Data.List              (elemIndex)
import           Data.Text              (Text, pack, unpack)
import           Reflex.Dom
import           Text.Read              (readMaybe)
import           Witherable             (catMaybes)

import           App.Common
import           Backend
import           Crypto                 (Zp(..), Fr, generateZp)
import qualified JS

data ButtonState = ButtonConnect | ButtonDeposit deriving Eq

depositForm :: MonadWidget t m => Dynamic t Bool -> m (Event t ())
depositForm dWalletConnected = do
  (eConn, dState) <- divClass "appformwrapper". divClass "form-block w-form" . el "form" $ mdo
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
    dDepositState <- holdDyn DepositInitial $ leftmost [DepositInProgress <$ eDeposit, eTxConstructed]
    let btnAttrs         = "class" =: "button w-button" <> "style" =: "cursor:pointer;"
        btnAttrsDisabled = "class" =: "button w-button please-wait" <> "disabled" =: ""
        mkDepositAttrs DepositInProgress = btnAttrsDisabled
        mkDepositAttrs _                 = btnAttrs
    eeBtn <- divClass "mainbuttonwrapper" $ dyn $ dWalletConnected <&> \case
      False -> do
        (e, _) <- elAttr' "a" ("class" =: "button w-button") $
          text "Connect to wallet"
        return (ButtonConnect <$ domEvent Click e)
      True -> do
        (e, _) <- elDynAttr' "a" (mkDepositAttrs <$> dDepositState) $ text "Deposit"
        return (ButtonDeposit <$ domEvent Click e)
    eBtn <- switchHold never eeBtn
    let
      eConnect = ffilter (== ButtonConnect) eBtn
      eDeposit = ffilter (== ButtonDeposit) eBtn
      depositArgs = zipDyn dToken dAmount
    eTxConstructed <- runDeposit elId key depositArgs (() <$ eDeposit)
    return (() <$ eConnect, dDepositState)
  dyn_ $ dState <&> (elAttr "div" ("class" =: "text-block-2" <> "id" =: elId) . text . ppState)
  return eConn
  where
    elId = "deposit-form-msg"
    initToken = TokenADA
    tokenInitAmount tok = let range = tokenToAmountRange tok in
      bool (head range) 0 (null range)
    ppState = \case
      DepositInitial    -> ""
      DepositInProgress -> "Constructing transaction..."
      DepositSuccess    -> "Transaction has been submitted!"
      DepositFailure    -> "Transaction was declined by the user."
    showToken = \case
      TokenADA -> "ADA"
      TokenMIX -> "MIX"
    tokenHint = "Choose a Cardano Blockchain native asset for a deposit. Non-ADA deposits require some ADA to cover the fees."
    amountHint = "Choose the amount of tokens to deposit."
    keyHint = pack $ "Save the secret key generated during a deposit. It will be used to withdraw tokens from the protocol. " ++ 
      "A copy of the Secret Key is automatically saved in your default Downloads folder."

-- Secret Key element
secretKeyInput :: MonadWidget t m => Event t () -> m (Dynamic t Fr)
secretKeyInput eDeposit = divClass "secretekeywrapper" . divClass "w-row" $ do
  eKey <- performEvent (liftIO generateZp <$ eDeposit)
  dKey <- holdDyn (Zp 0) eKey
  performEvent_ (fmap (JS.saveTextFile . toText) eKey)
  let keyId = "TextSecretKey"
  divClass colCls11 $ do
    let
      keyEnabledAttrs = "class" =: "textform" <> "id" =: keyId <> style
      keyDisabledAttrs = "class" =: "textform unable" <> style
      style = "style" =: "width: 300.66px; word-wrap: inherit; \
        \white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
    keyInpAttrs <- holdDyn keyDisabledAttrs (keyEnabledAttrs <$ eKey)
    keyInpText <- holdDyn "Please, save the secret key!" (fmap toText eKey)
    elDynAttr "div" keyInpAttrs $ dynText keyInpText
  divClass colCls4 $ do
    let
      copyEnabledAttrs = "class" =: "buttoncopy w-inline-block"
        <> "style" =: "cursor:pointer;"
      copyDisabledAttrs = "class" =: "buttoncopy unable w-inline-block"
    copyBtnAttrs <- holdDyn copyDisabledAttrs (copyEnabledAttrs <$ eKey)
    (e, _) <- elDynAttr' "a" copyBtnAttrs $ elAttr "img"
      ("src" =: "images/PictCopy.svg" <> "loading" =: "lazy" <>
        "width" =: "23" <> "class" =: "pictcopy") blank
    performEvent_ (JS.copyElemContent keyId <$ domEvent Click e)
  return dKey
  where
    colCls11 = "w-col w-col-11 w-col-small-11 w-col-tiny-11"
    colCls4 = "column-4 w-col w-col-1 w-col-small-1 w-col-tiny-1"

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
