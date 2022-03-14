module App where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Functor ((<&>))
import Data.List as L
import Data.Maybe hiding (catMaybes)
import Data.Text as T
import Data.Witherable (catMaybes)
import JS
import NamiJS as Nami
import Prelude as P
import Reflex.Dom
import System.Random
import Text.Read (readMaybe)

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

data FormState = Deposit | Withdraw deriving (Eq, Show)

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

data Token = TokenADA | TokenTest deriving (Eq, Show, Ord, Bounded, Enum)

tokenToAmountRange :: Token -> [Int]
tokenToAmountRange = \case
  TokenADA -> [20, 100, 200, 300]
  TokenTest -> [10, 20, 30]

data ButtonState = ButtonConnect | ButtonDeposit deriving Eq

depositForm :: MonadWidget t m => Dynamic t Bool -> m (Event t ())
depositForm dWalletConnected = do
  (eConn, eSubmitted) <- divClass "appformwrapper". divClass "form-block w-form"
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
      eTxSubmitted <- secretKeyInput (() <$ eDeposit)
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
      return (() <$ eConnect, eTxSubmitted)
  let elId = "deposit-form-msg"
  widgetHold_ blank ((elAttr "div" ("class" =: "text-block-2" <> "id" =: elId) $
    text "Сonfirm the transaction in your wallet") <$ eSubmitted)
  return eConn
  where
    initToken = TokenADA
    tokenInitAmount tok = let range = tokenToAmountRange tok in
      bool (P.head range) 0 (P.null range)
    showToken = \case
      TokenADA -> "ADA"
      TokenTest -> "Test"
    -- TODO: hint texts
    tokenHint = "It's a token"
    amountHint = "It's an amount"
    keyHint = "It's a secret key"

secretKeyInput :: MonadWidget t m => Event t () -> m (Event t ())
secretKeyInput eDeposit = divClass "secretekeywrapper" . divClass "w-row" $ do
  key :: Integer <- liftIO $ randomRIO (0, p^3)
  performEvent_ (JS.saveTextFile (toText key) <$ eDeposit)
  let keyId = "TextSecreteKey"
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
  runDeposit key eDeposit
  where
    colCls11 = "w-col w-col-11 w-col-small-11 w-col-tiny-11"
    colCls4 = "column-4 w-col w-col-1 w-col-small-1 w-col-tiny-1"
    p = 52435875175126190479447740508185965837690552500527637822603658699938581184513

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

inputTitle :: MonadWidget t m => Text -> Text -> m ()
inputTitle title hint = divClass "w-row" $ do
  divClass colCls6 . elAttr "label" ("class" =: "field-label") $ text title
  divClass colCls3 . elAttr "a" ("class" =: "buttoninfo w-inline-block" <>
    "title" =: hint <> "style" =: "cursor:pointer;") .
    elAttr "img" ("src" =: "/images/InfoPict.svg" <> "loading" =: "lazy" <>
      "width" =: "23" <> "class" =: "infopict") $ blank
  where
    colCls6 = "w-col w-col-6 w-col-medium-6 w-col-small-6 w-col-tiny-6"
    colCls3 = "column-3 w-col w-col-6 w-col-medium-6 w-col-small-6 w-col-tiny-6"

data WithdrawState
  = DepositNotFound | DepositFound | WithdrawInProgress
  | WithdrawOk | WithdrawError
  deriving (Eq, Show)

withdrawForm :: MonadWidget t m => m ()
withdrawForm = divClass "appformwrapper" . divClass "form-block w-form" .
  el "form" $ mdo
    inputTitle "Address" addrHint
    dAddr <- addressInput
    inputTitle "Secret Key" keyHint
    dKey <- fmap value $ inputElement $ def
      & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <>
        "maxlength" =: "256" <>
        "placeholder" =: "Please, enter the secret key of a deposit")
    let
      btnAttrs = "class" =: "button w-button" <> "style" =: "cursor:pointer;"
      btnAttrsDisabled = "class" =: "button w-button" <> "disabled" =: "" <>
        "style" =: "color: #c4c4c4;"
      btnHidden = "style" =: "display: none;"
      mkDepositAttrs (Just DepositFound) = btnHidden
      mkDepositAttrs (Just WithdrawInProgress) = btnHidden
      mkDepositAttrs _ = btnAttrs
      mkWithdrawAttrs (Just DepositFound) = btnAttrs
      mkWithdrawAttrs (Just WithdrawInProgress) = btnAttrsDisabled
      mkWithdrawAttrs _ = btnHidden
    dWithdrawState <- holdDyn Nothing $ leftmost
      [ Just DepositFound <$ ffilter isJust eFindDeposit
      , Just DepositNotFound <$ ffilter isNothing eFindDeposit
      , Just WithdrawInProgress <$ eWithdraw
      , Just WithdrawOk <$ ffilter id eWithdrawRes
      , Just WithdrawError <$ ffilter not eWithdrawRes ]
    eDeposit <- do
      (e, _) <- elAttr' "div" ("class" =: "mainbuttonwrapper") . elDynAttr "a"
        (mkDepositAttrs <$> dWithdrawState) $ text "Find deposit"
      return (domEvent Click e)
    eFindDeposit <- findDeposit dAddr dKey eDeposit
    dDepositRes <- holdDyn Nothing eFindDeposit
    eWithdraw <- do
      (e, _) <- elAttr' "div" ("class" =: "mainbuttonwrapper") . elDynAttr "a"
        (mkWithdrawAttrs <$> dWithdrawState) $ text "Withdraw"
      return (domEvent Click e)
    eWithdrawRes <- runWithdraw dAddr dKey eWithdraw
    blank
  where
    -- TODO: hint texts
    addrHint = "Address"
    keyHint = "Key"

addressInput :: MonadWidget t m => m (Dynamic t Text)
addressInput = divClass "w-row" $ mdo
  let elemId = "EnterWalletAddress"
  inp <- divClass "column-5 w-col w-col-9" $ inputElement $ def
    & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <>
      "id" =: elemId <> "maxlength" =: "256" <>
      "placeholder" =: "Please, enter a wallet address")
  eBtn <- divClass "column-4 w-col w-col-3" do
    (e, _) <- elAttr' "a" ("class" =: "buttonautofil w-button" <>
      "style" =: "cursor:pointer;") $ text "Autofill"
    return (domEvent Click e)
  performEvent_ (autofillAddr elemId <$ eBtn)
  return $ value inp

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

toText :: Show a => a -> Text
toText = T.pack . show

safeIndex :: [a] -> Int -> Maybe a
safeIndex zs n = guard (n >= 0) >> go zs n
  where
    go [] _ = Nothing
    go  (x:_) 0 = Just x
    go  (_:xs) i = go xs (pred i)

runDeposit :: MonadWidget t m => Integer -> Event t () -> m (Event t ())
runDeposit _ e = delay 1 e
-- отправляет запрос к бэкенду, который возвращает String.
-- Этот String отправляем в balanceTx -> signTx -> submitTx (это все функции API кошелька).

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Int, Text)))
findDeposit _dAddr _dKey e = (Just (1, "ADA") <$) <$> delay 1 e

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw _dAddr _dKey e = (True <$) <$> delay 2 e


