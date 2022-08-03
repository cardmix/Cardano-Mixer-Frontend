{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}

module Backend where

import           Data.Aeson           hiding (Value)
import qualified Data.Aeson           as JSON
import           Data.ByteString.Lazy (fromStrict)
import           Data.List            (find)
import qualified Data.Map
import           Data.Maybe           (isJust, isNothing, fromMaybe)
import           Data.Text            (Text, pack)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.Tuple.Extra     (snd3, thd3)
import           Data.Tuple.Select
import           Data.UUID            (nil)
import           GHC.Generics         (Generic)
import           Reflex.Dom           hiding (Value)
import           Servant.Reflex       (BaseUrl(..))
import           Witherable

import           Client
import           Crypto
import qualified JS              
import           MixerProofs.Groth16
import           Utils.Plutus

data DepositState = DepositInitial | DepositInProgress | DepositSigning | DepositSuccess | DepositFailure deriving Eq

data Token = TokenADA | TokenMIX deriving (Eq, Ord, Bounded, Enum)

instance Show Token where
  show TokenADA = "tADA"
  show TokenMIX = "tMIX"

tokenToAmountRange :: Token -> [Integer]
tokenToAmountRange = \case
  TokenADA -> [20, 30, 40, 50]
  TokenMIX -> [10_000, 20_000, 30_000]

allDepositOptions :: [(Token, Integer)]
allDepositOptions = map (TokenADA, ) (tokenToAmountRange TokenADA) ++ map (TokenMIX, ) (tokenToAmountRange TokenMIX)

toValue :: Token -> Integer -> Value
toValue TokenADA  k = Value $ Data.Map.singleton (CurrencySymbol "") (Data.Map.singleton (TokenName "") (k*2_000))
toValue TokenMIX k = Value $ Data.Map.singleton (CurrencySymbol "f21f17dd8a772ada1ead262823a224f1ec9dafad65dc6939cf3c4848")
  (Data.Map.singleton (TokenName "tMIX") (div k 500))

allDepositValues :: [Value]
allDepositValues = map (uncurry toValue) allDepositOptions

pabIP :: BaseUrl
pabIP = BasePath "http://127.0.0.1:9080"
  -- BasePath "https://m8cn730xz7.execute-api.eu-central-1.amazonaws.com/"

runDeposit :: MonadWidget t m => Text -> Dynamic t Fr -> Dynamic t (Token, Integer) ->
  Event t () -> m (Event t DepositState)
runDeposit elId _ _ eDeposit = do
  -- Update wallet address
  performEvent_ (JS.walletAddress "nami" elemId <$ eDeposit)
  dAddr <- fmap value $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: elemId

  -- Balance transaction and send it to the browser extension wallet
  let scAddr = "addr_test1qpcxwv6qtcx95emdfy780mqlyjvlk6fm4dugvm4cxuhndxzsj4s9a47vtwaldamtqrgpsjs0y65e78zuq3yxnf0j388sduypfw"
      val    = [("f21f17dd8a772ada1ead262823a224f1ec9dafad65dc6939cf3c4848", "744d4958", "1")]
      key    = pack . show $ fromZp (toZp 896294618941908241908250129 :: Fr)
      dp     = JS.DepositParams scAddr "5000000" val key
  eRunDeposit <- performEvent (flip (JS.runDeposit "nami") elemIdTx <$> (dp <$ updated dAddr))

  return $ leftmost [DepositSuccess <$ eRunDeposit, DepositFailure <$ eRunDeposit]
  where
    elemId = "deposit-addr-elem"
    elemIdTx = "tx-signed-elem"

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Token, Integer)))
findDeposit _ dKey e = do
  return never
  -- let client@ApiClient{..} = mkApiClient pabIP

  -- let
  --     dSecret = fmap (fromMaybe (DepositSecret (Zp 0) (Zp 0)) . readDepositSecret) dKey

  -- let dR1 = getR1 <$> dSecret
  --     dR2 = getR2 <$> dSecret
  --     dLeaf = zipDynWith mimcHash dR1 dR2
  -- eMixerStates <- getMixerStates client allDepositValues e
  -- let eFindDepositRes = findWithdrawPure <$> attachPromptlyDyn dLeaf eMixerStates

  -- performEvent_ $ fmap (JS.logInfo . pack . show) eFindDepositRes

  -- -- eNoResponse <- delay 160 e
  -- return $ leftmost [fmap (fmap ((!!) allDepositOptions . fst)) eFindDepositRes]
  

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw dAddr dKey e = do
  return never
  -- let client@ApiClient{..} = mkApiClient pabIP
  -- -- randomness :: ZKProofSecret <- liftIO $ generateProofSecret

  -- -- Connect to PAB
  -- dRespConnect <- connectToPAB client dAddr e
  -- let dPABWallet = fmap (Just . thd3) dRespConnect
  --     dAddrNum   = fmap snd3 dRespConnect
  -- let dSecret = fmap (fromMaybe (DepositSecret (Zp 0) (Zp 0)) . readDepositSecret) dKey

  -- let dR1 = getR1 <$> dSecret
  --     dR2 = getR2 <$> dSecret
  --     dLeaf = zipDynWith mimcHash dR1 dR2

  -- eMixerStates <- getMixerStates client allDepositValues $ () <$ updated dRespConnect
  -- let eFindDepositRes = findWithdrawPure <$> attachPromptlyDyn dLeaf eMixerStates
  --     eDepositFound   = catMaybes eFindDepositRes
  -- dVal        <- holdDyn (head allDepositValues) $ fmap ((!!) allDepositValues . fst) eDepositFound
  -- dMixerState <- holdDyn [] $ fmap snd eDepositFound

  -- let 
  --     -- dWires    = mkProveArgs <$> dAddrNum <*> dSecret <*> dMixerState
  --     -- dLastDeposit = fmap sel1 dWires
  --     -- _            = fmap sel2 dWires
  --     -- dPubIns      = fmap sel3 dWires
  --     -- dPrivIns     = fmap sel4 dWires
  --     -- dSubsIns     = zipDynWith (++) dPubIns dPrivIns

  -- -- creating input map
  -- let 
  --     -- dSubsMap    = fmap toInputMap dSubsIns
  --     -- eProveStart = attachPromptlyDynWith const dSubsMap $ updated dMixerState

  -- -- performEvent_ (JS.fillProof elemId <$> eProveStart)
  -- performEvent_ (JS.fillProof elemId <$> ("" <$ eDepositFound))
  -- dProofText <- fmap value $ inputElement $ def
  --   & initialAttributes .~ "style" =: "display:none;" <> "id" =: elemId
  -- let dProof = fmap (unProofJS . fromMaybe (ProofJS $ Proof O O O) . decode . fromStrict . encodeUtf8) dProofText
  --     -- dParams = WithdrawParams <$> dAddr <*> dVal <*> dLastDeposit <*> fmap PublicInputs dPubIns <*> dProof

  -- aRespWithdraw <- activateRequest (fmap Right (ContractActivationArgs (FrontendContracts MixerUse) <$> dPABWallet)) (() <$ updated dProof)
  -- let aRespWithdraw' = catMaybes $ makeResponse <$> aRespWithdraw
  -- cidMixerWithdraw <- fmap Right <$> holdDyn defCID aRespWithdraw'
  -- eRespWithdraw <- endpointRequest cidMixerWithdraw (pure . pure $ "withdraw") (pure . Right $ JSON.toJSON ("" :: Text)) (() <$ updated cidMixerWithdraw)
  -- let eRespWithdraw' = catMaybes $ makeResponse <$> eRespWithdraw
  -- eWithdrawMessage <- getStatus client 50 cidMixerWithdraw eRespWithdraw'

  -- -- eNoResponse <- delay 180 e
  -- -- return $ leftmost [True <$ eRespWithdraw, False <$ ffilter isNothing eFindDepositRes]
  -- return $ leftmost [fmap (== ("RelayRequestAccepted" :: Text)) eWithdrawMessage, False <$ ffilter isNothing eFindDepositRes]
  -- where
  --   elemId = "proof-elem"
  --   -- testSAS = ShieldedAccountSecret (toZp 48081304776541705762740047315054676823791585112593769402696472791053032063395)
  --   --     (toZp 23242094360494712196167729465930882794461214075280956494392702130165068291126)
  --   --     (toZp 5355364785739370808595236295435031456604123972461255344590972311627062382674)
  --   -- mkProveArgs a ds state = computeWithdrawWires a ds testSAS state

fromJSONValue :: FromJSON b => JSON.Value -> Maybe b
fromJSONValue v = case fromJSON v of
  Success a -> Just a
  _         -> Nothing
