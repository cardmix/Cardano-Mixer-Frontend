module Backend.Types where

import           Data.Text                  (Text, unpack)
import           Text.Read                  (readMaybe)

import           Crypto                     (toZp)
import           MixerProofs.SigmaProtocol  (ExpField)


class MakeStatus a where
  makeStatus :: Text -> a
  statusElement :: Text

-----------------------------------------------------------------------------------------------------------

data RunDepositStatus = RunDepositInitial | RunDepositOK ExpField | RunDepositInProgress
  | RunDepositWalletConnectError| RunDepositWalletFundsError | RunDepositWalletSignError | RunDepositWalletSubmitError
  deriving Eq

readSecretFromRunDepositStatus :: RunDepositStatus -> Maybe ExpField
readSecretFromRunDepositStatus = \case
  RunDepositOK secret -> Just secret
  _                   -> Nothing

instance MakeStatus RunDepositStatus where
  makeStatus = \case
    "in_progress"     -> RunDepositInProgress
    "error_walletAPI" -> RunDepositWalletConnectError
    "error_utxos"     -> RunDepositWalletFundsError
    "error_sign"      -> RunDepositWalletSignError
    "error_submit"    -> RunDepositWalletSubmitError
    txt -> case readMaybe (unpack txt) of
          Just secret -> RunDepositOK $ toZp secret
          Nothing     -> RunDepositInitial
  statusElement = "RunDepositStatus"

instance Show RunDepositStatus where
  show = \case
    RunDepositInitial            -> ""
    RunDepositOK _               -> "Transaction has been submitted!"
    RunDepositInProgress         -> "Please sign the transaction in your wallet"
    RunDepositWalletConnectError -> "Could not connect to a wallet!"
    RunDepositWalletFundsError   -> "Not enough funds to make a deposit!"
    RunDepositWalletSignError    -> "Transaction was declined by the user."
    RunDepositWalletSubmitError  -> "Transaction was declined by the wallet."

-----------------------------------------------------------------------------------------------------------

data FindDepositStatus = FindDepositInitial | FindDepositOK (Text, Integer) | FindDepositInProgress
  | FindDepositAPIRequestError | FindDepositIncorrectProofError
  deriving Eq

readDepositFromFindDepositStatus :: FindDepositStatus -> Maybe (Text, Integer)
readDepositFromFindDepositStatus = \case
  FindDepositOK d -> Just d
  _               -> Nothing

instance Show FindDepositStatus where
  show = \case
    FindDepositInitial             -> ""
    FindDepositOK (txt, n)         -> "Found a deposit of " ++ show n ++ " " ++ show txt ++ "."
    FindDepositInProgress          -> "Searching for a deposit..."
    FindDepositAPIRequestError     -> "A connection to the relayer could not be established."
    FindDepositIncorrectProofError -> "No deposits were found for this key."

-----------------------------------------------------------------------------------------------------------

data RunWithdrawStatus = RunWithdrawInitial | RunWithdrawOK | RunWithdrawInProgress
  | RunWithdrawAPIRequestError | RunWithdrawIncorrectProofError
  deriving Eq

instance Show RunWithdrawStatus where
  show = \case
    RunWithdrawInitial             -> ""
    RunWithdrawOK                  -> "Transaction has been confirmed on the blockchain."
    RunWithdrawInProgress          -> "Transaction has been submitted. Waiting for confirmation..."
    RunWithdrawAPIRequestError     -> "A connection to the relayer could not be established."
    RunWithdrawIncorrectProofError -> "No deposits were found for this key."