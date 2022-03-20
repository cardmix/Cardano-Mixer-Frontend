module App.Withdraw where

import App.Common
import Backend
import Data.Bool
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text as T
import JS
import Prelude as P
import Reflex.Dom

data WithdrawState
  = DepositNotFound | DepositFound Int Text | WithdrawInProgress
  | WithdrawOk | WithdrawError
  deriving (Eq, Show)

withdrawForm :: MonadWidget t m => m ()
withdrawForm = do
  dState <- divClass "appformwrapper" . divClass "form-block w-form" . el "form"
    $ mdo
      inputTitle "Address" addrHint
      dAddr <- addressInput
      inputTitle "Secret Key" keyHint
      dKey <- fmap value $ inputElement $ def
        & initialAttributes .~ ("class" =: "w-input" <> "type" =: "text" <>
          "maxlength" =: "256" <>
          "placeholder" =: "Please, enter the secret key of a deposit")
      let
        btnAttrs = "class" =: "button w-button" <> "style" =: "cursor:pointer;"
        btnAttrsDisabled = "class" =: "button w-button please-wait" <>
          "disabled" =: ""
        btnHidden = "style" =: "display: none;"
        mkDepositEvent (Just (n, txt)) = Just $ DepositFound n txt
        mkDepositEvent _ = Just DepositNotFound
        mkDepositAttrs (Just (DepositFound _ _)) = btnHidden
        mkDepositAttrs (Just WithdrawInProgress) = btnHidden
        mkDepositAttrs _ = btnAttrs
        mkWithdrawAttrs (Just (DepositFound _ _)) = btnAttrs
        mkWithdrawAttrs (Just WithdrawInProgress) = btnAttrsDisabled
        mkWithdrawAttrs _ = btnHidden
      dWithdrawState <- holdDyn Nothing $ leftmost
        [ mkDepositEvent <$> eFindDeposit
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
      return dWithdrawState
  dyn_ $ dState <&> maybe blank (divClass "textinfoaboutwithdraw" . text . ppState)
  where
    ppState = \case
      DepositNotFound -> "Error: deposit is not found"
      DepositFound n txt -> toText n <> " " <> txt <> " is found"
      WithdrawInProgress -> "Withdrawing takes time, please, wait!"
      WithdrawOk -> "Withdrawing is finished!"
      WithdrawError -> "Error: withdrawing failed"
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
