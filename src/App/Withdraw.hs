module App.Withdraw where

import           Data.Bool           (bool)
import           Data.Either         (fromRight)
import           Data.Maybe          (isJust)
import           Data.Text           (Text, unpack)
import           Reflex.Dom
import           Text.Read           (readMaybe)

import           App.Common
import           Backend             (FindDepositStatus (..), RunWithdrawStatus (..), findDeposit, runWithdraw, readDepositFromFindDepositStatus)
import           Crypto              (Zp(..), toZp)
import qualified JS
import           WebPage.Basic       (inputTitle, appButton, appButtonAttrs, appButtonAttrsDisabled, elementResultJS)


data WithdrawState
  = FindInProgress | DepositNotFound | DepositFound Text Integer | WithdrawInProgress
  | WithdrawOk | WithdrawError
  deriving (Eq, Show)

withdrawForm :: MonadWidget t m => Dynamic t Text -> m ()
withdrawForm dWalletName = do
  dState <- divClass "appformwrapper" . divClass "form-block w-form" . el "form"
    $ mdo
      let addrHint = "The recipient's wallet address in bech32 format, e.g. addr_test1abc123..."
      inputTitle "Address" addrHint
      dAddrBech32 <- addressInput dWalletName
      let elemIdBytes = "WalletAddressBytes"
      dAddr <- elementResultJS elemIdBytes id
      performEvent_ (tagPromptlyDyn (flip JS.walletAddressBech32ToBytes elemIdBytes <$> dAddrBech32) $ updated dAddrBech32)
      
      let keyHint  = "A Secret Key that was generated during a deposit."
      inputTitle "Secret Key" keyHint
      dKeyText <- fmap value $ inputElement $ def
        & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <> "maxlength" =: "256" <>
          "placeholder" =: "Please, enter the secret key of a deposit")
      let dKey = maybe (Zp 0) toZp . readMaybe . unpack <$> dKeyText      
      
      let mkFindLinkAttrs s = bool appButtonAttrs appButtonAttrsDisabled (s == FindDepositInProgress)
      eDeposit <- appButton "Find deposit" (not <$> dFindDepositResult) mkFindLinkAttrs dFindDepositStatus

      eFindDepositStatus <- findDeposit dKey eDeposit
      dFindDepositStatus <- holdDyn FindDepositInitial eFindDepositStatus
      let dFindDepositResult = isJust . readDepositFromFindDepositStatus <$> dFindDepositStatus

      let mkWithdrawLinkAttrs s = bool appButtonAttrs appButtonAttrsDisabled (s == RunWithdrawInProgress)
      eWithdraw <- appButton "Withdraw" dFindDepositResult mkWithdrawLinkAttrs dRunWithdrawStatus

      let dAddrExp = byteStringToZp . dropByteString 1 . fromRight emptyByteString . tryDecode <$> dAddr
      eRunWithdrawStatus <- runWithdraw dAddr dKey dAddrExp eWithdraw
      dRunWithdrawStatus <- holdDyn RunWithdrawInitial eRunWithdrawStatus

      holdDyn "" $ leftmost [toText <$> eFindDepositStatus, toText <$> eRunWithdrawStatus]
  dyn_ $ divClass "textinfoaboutwithdraw" . text <$> dState

-- Address input element
addressInput :: MonadWidget t m => Dynamic t Text -> m (Dynamic t Text)
addressInput dWalletName = divClass "w-row" $ mdo
  let elemId = "EnterWalletAddress"
  inp <- divClass "column-5 w-col w-col-9" $ inputElement $ def
    & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <>
      "id" =: elemId <> "maxlength" =: "256" <>
      "placeholder" =: "Please, enter a wallet address")
  eBtn <- divClass "column-4 w-col w-col-3" do
    (e, _) <- elAttr' "a" ("class" =: "buttonautofil w-button" <>
      "style" =: "cursor:pointer;") $ text "Autofill"
    return (domEvent Click e)
  performEvent_ (tagPromptlyDyn (flip JS.walletAddressBech32 elemId <$> dWalletName) eBtn)
  return $ value inp
