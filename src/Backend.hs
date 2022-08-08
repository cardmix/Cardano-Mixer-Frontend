module Backend (module Backend, module Backend.Types) where

import           Control.Monad.IO.Class     (MonadIO(..))
import           Data.Bifunctor             (bimap)
import           Data.Text                  (Text)
import           Reflex.Dom                 hiding (Value)
import           Servant.Reflex             (BaseUrl(..))

import           App.Common                 (toText, eventMaybe)
import           Backend.Types
import           Client
import           Crypto
import qualified JS
import           MixerProofs.SigmaProtocol
import           WebPage.Basic              (elementResultJS)


pabIP :: BaseUrl
pabIP = BasePath "http://127.0.0.1:9080"
  -- BasePath "https://m8cn730xz7.execute-api.eu-central-1.amazonaws.com/"

runDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t MixerDepositInstanceFrontend ->
  Event t () -> m (Event t RunDepositStatus)
runDeposit dWalletName dMDIF e = do
  let elemId = statusElement @RunDepositStatus
  performEvent_ (JS.setInputValue elemId "in_progress" <$ e)
  -- performEvent (liftIO . JS.logInfo <$> e')

  eSecret <- performEvent (liftIO (generateZp :: IO ExpField) <$ e)
  let eKey     = attachPromptlyDynWith (\g s -> (pow g (fromZp s), s)) (mdifGenerator <$> dMDIF) eSecret
      eKeyText = fmap (bimap (toText . fromZp) (toText . fromZp)) eKey

  let dAddr   = mdifCurrentDepositAddress <$> dMDIF
      dNonADA = mdifCurrentDepositNonADA <$> dMDIF
      dADA    = mdifCurrentDepositADA <$> dMDIF
      eDP     = attachPromptlyDynWith (\f x -> f x) (JS.DepositParams <$> dAddr <*> dADA <*> dNonADA) eKeyText

  -- Balance transaction and send it to the browser extension wallet
  let eRunDepositAction = attachPromptlyDynWith (\w dp -> JS.runDeposit w dp elemId) dWalletName eDP
  performEvent_ eRunDepositAction

  updated <$> elementResultJS elemId (makeStatus @RunDepositStatus)

findDeposit :: MonadWidget t m => Dynamic t ExpField
  -> Event t () -> m (Event t FindDepositStatus)
findDeposit dKey e = do
  let ApiClient{..} = mkApiClient pabIP

  keysRespMaybe <- fmap makeResponse <$> keysRequest e
  let (keysResp, keysRespError) = eventMaybe FindDepositAPIRequestError keysRespMaybe

  let eFindDepositMaybe = attachPromptlyDynWith apiSigmaProtocolFindDeposit dKey keysResp
      (eFindDepositResult, eFindDepositResultError) = eventMaybe FindDepositIncorrectProofError eFindDepositMaybe

  let f = (\mif -> (mifTokenName mif, mifQuantity mif)) . krMixerInstanceFrontend
  return $ leftmost [FindDepositOK . f <$> eFindDepositResult, keysRespError, eFindDepositResultError, FindDepositInProgress <$ e]

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t ExpField -> Dynamic t ExpField
  -> Event t () -> m (Event t RunWithdrawStatus)
runWithdraw dAddr dKey dAddrExp e = do
  let ApiClient{..} = mkApiClient pabIP
      dInput = SigmaProtocolGenerateProofInput 10 <$> fmap Left dAddr <*> dKey <*> dAddrExp -- TODO: the number of alternative keys should be a parameter!

  keysRespMaybe <- fmap makeResponse <$> keysRequest e
  let (keysResp, keysRespError) = eventMaybe RunWithdrawAPIRequestError keysRespMaybe

  let eGenerateProofMaybe = attachPromptlyDynWith apiSigmaProtocolGenerateProof dInput keysResp
  eGenerateProofResultMaybe <- performEvent (fmap liftIO eGenerateProofMaybe)
  let (eGenerateProofResult, eGenerateProofResultError) = eventMaybe RunWithdrawIncorrectProofError eGenerateProofResultMaybe

  dWithdrawRequest <- holdDyn (Left "") (fmap Right eGenerateProofResult)
  eWithdrawRequestMaybe <- fmap makeResponse <$> withdrawRequest dWithdrawRequest (() <$ updated dWithdrawRequest)
  let (eWithdrawRequest, eWithdrawRequestError) = eventMaybe RunWithdrawAPIRequestError eWithdrawRequestMaybe

  return $ leftmost [RunWithdrawOK <$ eWithdrawRequest, keysRespError, eGenerateProofResultError, eWithdrawRequestError, RunWithdrawInProgress <$ e]
