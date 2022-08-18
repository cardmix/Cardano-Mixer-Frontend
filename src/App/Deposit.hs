module App.Deposit where

import           Data.Bool                 (bool)
import           Data.Functor              ((<&>))
import           Data.List                 (find)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text, pack)
import           Reflex.Dom

import           App.Common
import           Backend
import           Crypto                    (Zp(..), fromZp)
import qualified JS
import           MixerProofs.SigmaProtocol (MixerDepositInstanceFrontend (..))
import           WebPage.Basic             (selectInput, inputTitle, appButton, appButtonAttrsDisabled, appButtonAttrs)


data ButtonState = ButtonConnect | ButtonDeposit deriving Eq

depositPools :: [MixerDepositInstanceFrontend]
depositPools = [MixerDepositInstanceFrontend (Zp 3) "addr_test1qpcxwv6qtcx95emdfy780mqlyjvlk6fm4dugvm4cxuhndxzsj4s9a47vtwaldamtqrgpsjs0y65e78zuq3yxnf0j388sduypfw" "5000000" [("f21f17dd8a772ada1ead262823a224f1ec9dafad65dc6939cf3c4848", "744d4958", "1")] "ADA" 10]

depositPoolFromForm :: Text -> Integer -> Maybe MixerDepositInstanceFrontend
depositPoolFromForm token amt = find (\a -> mdifTokenName a == token && mdifQuantity a == amt) depositPools

tokenToAmountRange :: Text -> [Integer]
tokenToAmountRange token =
  let mdifs = filter (\a -> mdifTokenName a == token) depositPools
  in map mdifQuantity mdifs

tokenInitAmount :: Text -> Integer
tokenInitAmount token =
  let range = tokenToAmountRange token in
  bool (head range) 0 (null range)

depositForm :: MonadWidget t m => Dynamic t Text -> Dynamic t Bool -> m (Event t ())
depositForm dWalletName dWalletConnected = do
  (eConn, dState) <- divClass "appformwrapper". divClass "form-block w-form" . el "form" $ mdo
    let initToken = "ADA"
        tokenHint = "Choose a Cardano Blockchain native asset for a deposit. Non-ADA deposits require some ADA to cover the fees."
    eToken <- selectInput "Token" tokenHint id initToken (map mdifTokenName depositPools)
    dToken <- holdDyn initToken eToken

    let amountHint = "Choose the amount of tokens to deposit."
        amountInput tok = selectInput "Amount" amountHint toText (tokenInitAmount tok) (tokenToAmountRange tok)
    eAmount <- switchHold never =<< dyn (amountInput <$> dToken)
    dAmount <- holdDyn (tokenInitAmount initToken) $ leftmost [eAmount, tokenInitAmount <$> eToken]

    let keyHint = pack $ "Save the secret key generated during a deposit. It will be used to withdraw tokens from the protocol. " ++
          "A copy of the Secret Key is automatically saved in your default Downloads folder."
    inputTitle "Secret Key" keyHint
    secretKeyInput eRunDeposit

    let mkDepositLinkAttrs s = bool appButtonAttrs appButtonAttrsDisabled (s == RunDepositInProgress)
    eConnect <- appButton "Connect to wallet" (not <$> dWalletConnected) (const appButtonAttrs) dDepositState
    eDeposit <- appButton "Deposit" dWalletConnected mkDepositLinkAttrs dDepositState

    -- TODO: replace forJust here (even though we do not expect a Nothing here)
    let dMDIF = fromJust <$> (depositPoolFromForm <$> dToken <*> dAmount)
    eRunDeposit <- runDeposit dWalletName dMDIF (() <$ eDeposit)
    dDepositState <- holdDyn RunDepositInitial eRunDeposit

    return (() <$ eConnect, dDepositState)
  dyn_ $ dState <&> (elAttr "div" ("class" =: "text-block-2" <> "id" =: "DepositFormMessage") . text . toText)
  return eConn

-- Secret Key element
secretKeyInput :: MonadWidget t m => Event t RunDepositStatus -> m ()
secretKeyInput eRunDeposit = divClass "secretekeywrapper" . divClass "w-row" $ do
  let secretHint = "Please, save the secret key!"
      eSecretTextMaybe = fmap (toText . fromZp) . readSecretFromRunDepositStatus <$>  eRunDeposit
      (eSecretText, eSecretTextError) = eventMaybe secretHint eSecretTextMaybe
  dSecretText <- holdDyn secretHint $ leftmost [eSecretText, eSecretTextError]

  performEvent_ (fmap JS.saveTextFile eSecretText)

  let keyId = "TextSecretKey"
  divClass "w-col w-col-11 w-col-small-11 w-col-tiny-11" $ do
    let style = "style" =: "width: 300.66px; word-wrap: inherit; \
          \white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
        keyEnabledAttrs = "class" =: "textform" <> "id" =: keyId <> style
        keyDisabledAttrs = "class" =: "textform unable" <> style
    keyInpAttrs <- holdDyn keyDisabledAttrs $ leftmost [keyEnabledAttrs <$ eSecretText, keyDisabledAttrs <$ eSecretTextError]
    elDynAttr "div" keyInpAttrs $ dynText dSecretText

  divClass "column-4 w-col w-col-1 w-col-small-1 w-col-tiny-1" $ do
    let copyEnabledAttrs = "class" =: "buttoncopy w-inline-block" <> "style" =: "cursor:pointer;"
        copyDisabledAttrs = "class" =: "buttoncopy unable w-inline-block"
    copyBtnAttrs <- holdDyn copyDisabledAttrs $ leftmost [copyEnabledAttrs <$ eSecretText, copyDisabledAttrs <$ eSecretTextError]
    (e, _) <- elDynAttr' "a" copyBtnAttrs $ elAttr "img"
      ("src" =: "images/PictCopy.svg" <> "loading" =: "lazy" <>
        "width" =: "23" <> "class" =: "pictcopy") blank
    performEvent_ (JS.copyElemContent keyId <$ domEvent Click e)

