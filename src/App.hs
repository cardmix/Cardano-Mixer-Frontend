module App where

import Control.Monad
import Data.Bool
import Data.Functor ((<&>))
import Data.List as L
import Data.Text as T
import Data.Witherable (catMaybes)
import JS
import Prelude as P
import Reflex.Dom
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
    dWalletConnected <- holdDyn False never
    eDeposit <- buttonSwitch Deposit dFormState
    divClass "w-col w-col-8 w-col-medium-8 w-col-small-8 w-col-tiny-8" .
      divClass "appformwrapper" . divClass "form-block w-form" .
        el "form" . dyn_ $ dFormState <&> \case
          Deposit -> depositForm dWalletConnected
          Withdraw -> withdrawForm
    eWithdraw <- buttonSwitch Withdraw dFormState
    blank

data Token = TokenADA | TokenTest deriving (Eq, Show, Ord, Bounded, Enum)

tokenToAmountRange :: Token -> [Int]
tokenToAmountRange = \case
  TokenADA -> [20, 100, 200, 300]
  TokenTest -> [10, 20, 30]

depositForm :: MonadWidget t m => Dynamic t Bool -> m ()
depositForm dWalletConnected = do
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
  secretKeyInput
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

secretKeyInput :: MonadWidget t m => m ()
secretKeyInput = divClass "secretekeywrapper" . divClass "w-row" $ do
  let keyId = "TextSecreteKey"
  divClass colCls11 $ do
    let
      keyEnabledAttrs = "class" =: "textform" <> "id" =: keyId
      -- keyDisabledAttrs = "class" =: "textform unable"
      keyInpAttrs = pure keyEnabledAttrs -- TODO
    elDynAttr "div" keyInpAttrs $ text "Please, save the secret key!"
  divClass colCls4 $ do
    let
      copyEnabledAttrs = "class" =: "buttoncopy w-inline-block"
        <> "style" =: "cursor:pointer;"
      -- copyDisabledAttrs = "class" =: "buttoncopy unable w-inline-block"
      copyBtnAttrs = pure copyEnabledAttrs -- TODO
    (e, _) <- elDynAttr' "a" copyBtnAttrs $ elAttr "img"
      ("src" =: "/images/PictCopy.svg" <> "loading" =: "lazy" <>
        "width" =: "23" <> "class" =: "pictcopy") blank
    performEvent_ (copyElemContent keyId <$ domEvent Click e)
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

withdrawForm :: MonadWidget t m => m ()
withdrawForm = do
  inputTitle "Address" addrHint
  dAddr <- addressInput
  inputTitle "Secret Key" keyHint
  dKey <- inputElement $ def
    & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <>
      "maxlength" =: "256" <>
      "placeholder" =: "Please, enter the secret key of a deposit")
  dDepositFound <- holdDyn False never
  let
    depBtnAttrs = "class" =: "button w-button" <> "style" =: "cursor:pointer;"
    btnAttrsHidden = "style" =: "display:none;"
  eDepBtn <- elDynAttr "a" (bool depBtnAttrs btnAttrsHidden <$> dDepositFound)
    $ text "Find deposit"
  -- TODO: find deposit <-> withdraw logic
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

