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
import           Data.Witherable
import           GHC.Generics         (Generic)
import           Reflex.Dom           hiding (Value)
import           Servant.Reflex       (BaseUrl(..))

import           Client
import           Crypto
import           JS
import           MixerContractParams
import           MixerProofs          (computeWithdrawWires)
import           MixerState           (MixerState, getMerkleLeafNumber)
import           MixerUserData
import           NamiJS               as Nami

data DepositState = DepositInitial | DepositInProgress | DepositSuccess | DepositFailure deriving Eq

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

defCID :: ContractInstanceId
defCID = ContractInstanceId nil

runDeposit :: MonadWidget t m => Text -> Dynamic t DepositSecret -> Dynamic t (Token, Integer) ->
  Event t () -> m (Event t DepositState)
runDeposit elId dKey dReq eDeposit = do
  let client@ApiClient{..} = mkApiClient pabIP

  -- Update wallet address
  performEvent_ (autofillAddr elemId <$ eDeposit)
  dAddr <- fmap value $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: elemId

  -- Connect to PAB
  dRespConnect <- connectToPAB client dAddr $ () <$ updated dAddr
  let dPABWallet = fmap (Just . thd3) dRespConnect

  -- Deposit
  aRespDeposit <- activateRequest (fmap Right (ContractActivationArgs (FrontendContracts MixerUse) <$> dPABWallet))
    (() <$ updated dRespConnect)
  let aRespDeposit' = catMaybes $ makeResponse <$> aRespDeposit
  cidMixerUse <- fmap Right <$> holdDyn defCID aRespDeposit'
  eRespDeposit <- endpointRequest cidMixerUse (pure . pure $ "deposit") (mkDepositParams <$> dKey <*> dAddr <*> dReq) (() <$ updated cidMixerUse)
  let eRespDeposit' = catMaybes $ makeResponse <$> eRespDeposit
  sRespDeposit' <- fmap getTxUnsignedCW <$> getStatus client 30 cidMixerUse eRespDeposit'
  performEvent_ (Nami.runDeposit elId elemIdTx <$> sRespDeposit')

  dTx <- fmap value $ inputElement $ def & initialAttributes .~ "style" =: "display:none;" <> "id" =: elemIdTx
  -- Catching signing failure event
  let eSigningFailure = ffilter (== "") $ updated dTx
      eSingingSuccess = ffilter (/= "") $ updated dTx

  -- Deposit submit
  aRespReturn <- activateRequest (fmap Right (ContractActivationArgs (FrontendContracts MixerUse) <$> dPABWallet)) (() <$ eSingingSuccess)
  let aRespReturn' = catMaybes $ makeResponse <$> aRespReturn
  cidMixerReturn <- fmap Right <$> holdDyn defCID aRespReturn'
  eRespReturn <- endpointRequest cidMixerReturn (pure . pure $ "deposit-submit") (Right . JSON.toJSON <$> dTx) (() <$ updated cidMixerReturn)

  -- eNoResponse <- delay 120 eDeposit
  return $ leftmost [DepositSuccess <$ eRespReturn, DepositFailure <$ eSigningFailure]
  where
    elemId = "deposit-addr-elem"
    elemIdTx = "tx-signed-elem"
    mkDepositParams key addr (token, amount) = Right $ JSON.toJSON $ DepositParams addr (toValue token amount) (mimcHash (getR1 key) (getR2 key))

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Token, Integer)))
findDeposit _ dKey e = do
  let client@ApiClient{..} = mkApiClient pabIP

  let
      dSecret = fmap (fromMaybe (DepositSecret (Zp 0) (Zp 0)) . readDepositSecret) dKey

  let dR1 = getR1 <$> dSecret
      dR2 = getR2 <$> dSecret
      dLeaf = zipDynWith mimcHash dR1 dR2
  eMixerStates <- getMixerStates client allDepositValues e
  let eFindDepositRes = findWithdrawPure <$> attachPromptlyDyn dLeaf eMixerStates

  performEvent_ $ fmap (logInfo . pack . show) eFindDepositRes

  -- eNoResponse <- delay 160 e
  return $ leftmost [fmap (fmap ((!!) allDepositOptions . fst)) eFindDepositRes]

findWithdrawPure :: (Fr, [MixerState]) -> Maybe (Int, MixerState)
findWithdrawPure (l, states) = find (\s -> isJust $ getMerkleLeafNumber (snd s) l) $ zip [0..] states

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw dAddr dKey e = do
  let client@ApiClient{..} = mkApiClient pabIP
  -- randomness :: ZKProofSecret <- liftIO $ generateProofSecret

  -- Connect to PAB
  dRespConnect <- connectToPAB client dAddr e
  let dPABWallet = fmap (Just . thd3) dRespConnect
      dAddrNum   = fmap snd3 dRespConnect
  let dSecret = fmap (fromMaybe (DepositSecret (Zp 0) (Zp 0)) . readDepositSecret) dKey

  let dR1 = getR1 <$> dSecret
      dR2 = getR2 <$> dSecret
      dLeaf = zipDynWith mimcHash dR1 dR2

  eMixerStates <- getMixerStates client allDepositValues $ () <$ updated dRespConnect
  let eFindDepositRes = findWithdrawPure <$> attachPromptlyDyn dLeaf eMixerStates
      eDepositFound   = catMaybes eFindDepositRes
  dVal        <- holdDyn (head allDepositValues) $ fmap ((!!) allDepositValues . fst) eDepositFound
  dMixerState <- holdDyn [] $ fmap snd eDepositFound

  let dWires    = mkProveArgs <$> dAddrNum <*> dSecret <*> dMixerState
      dLastDeposit = fmap sel1 dWires
      _            = fmap sel2 dWires
      dPubIns      = fmap sel3 dWires
      dPrivIns     = fmap sel4 dWires
      dSubsIns     = zipDynWith (++) dPubIns dPrivIns

  -- creating input map
  let dSubsMap    = fmap toInputMap dSubsIns
      eProveStart = attachPromptlyDynWith const dSubsMap $ updated dMixerState

  performEvent_ (fillProof elemId <$> eProveStart)
  dProofText <- fmap value $ inputElement $ def
    & initialAttributes .~ "style" =: "display:none;" <> "id" =: elemId
  let dProof = fmap (unProofJS . fromMaybe (ProofJS $ Proof O O O) . decode . fromStrict . encodeUtf8) dProofText
      dParams = WithdrawParams <$> dAddr <*> dVal <*> dLastDeposit <*> fmap PublicInputs dPubIns <*> dProof

  aRespWithdraw <- activateRequest (fmap Right (ContractActivationArgs (FrontendContracts MixerUse) <$> dPABWallet)) (() <$ updated dProof)
  let aRespWithdraw' = catMaybes $ makeResponse <$> aRespWithdraw
  cidMixerWithdraw <- fmap Right <$> holdDyn defCID aRespWithdraw'
  eRespWithdraw <- endpointRequest cidMixerWithdraw (pure . pure $ "withdraw") (Right . JSON.toJSON <$> dParams) (() <$ updated cidMixerWithdraw)
  let eRespWithdraw' = catMaybes $ makeResponse <$> eRespWithdraw
  eWithdrawMessage <- getStatus client 50 cidMixerWithdraw eRespWithdraw'

  -- eNoResponse <- delay 180 e
  -- return $ leftmost [True <$ eRespWithdraw, False <$ ffilter isNothing eFindDepositRes]
  return $ leftmost [fmap (== ("RelayRequestAccepted" :: Text)) eWithdrawMessage, False <$ ffilter isNothing eFindDepositRes]
  where
    elemId = "proof-elem"
    testSAS = ShieldedAccountSecret (toZp 48081304776541705762740047315054676823791585112593769402696472791053032063395)
        (toZp 23242094360494712196167729465930882794461214075280956494392702130165068291126)
        (toZp 5355364785739370808595236295435031456604123972461255344590972311627062382674)
    mkProveArgs a ds state = computeWithdrawWires a ds testSAS state

------------------------------------------------------------------

connectToPAB :: forall t m . MonadWidget t m => ApiClient t m -> Dynamic t Text -> Event t () -> m (Dynamic t (PaymentPubKeyHash, Fr, Wallet))
connectToPAB ApiClient{..} dAddr e = do
  aRespConnect <- activateRequest (pure . pure $ ContractActivationArgs (FrontendContracts ConnectToPAB) Nothing) e
  let aRespConnect' = catMaybes $ makeResponse <$> aRespConnect
  cidPAB <- fmap Right <$> holdDyn defCID aRespConnect'
  eRespConnect <- endpointRequest cidPAB (pure . pure $ "connect-to-pab")
    (fmap (Right . String) dAddr) (() <$ updated cidPAB)
  let eRespConnect' = catMaybes $ makeResponse <$> eRespConnect
  sRespConnect <- statusRequest cidPAB eRespConnect'
  let sRespConnect' = mapMaybe ((fromJSONValue :: JSON.Value -> Maybe (PaymentPubKeyHash, Fr, Wallet)) . observableState . cicCurrentState) (
                  catMaybes $ makeResponse <$> sRespConnect)
  holdDyn (PaymentPubKeyHash $ PubKeyHash "", Zp 0, Wallet "") sRespConnect'

getMixerStates :: forall t m . MonadWidget t m => ApiClient t m -> [Value] -> Event t () -> m (Event t [MixerState])
getMixerStates client@ApiClient{..} vals e = do
  aRespState <- activateRequest (pure . pure $ ContractActivationArgs (FrontendContracts MixerStateQuery) Nothing) e
  let aRespState' = catMaybes $ makeResponse <$> aRespState
  cidState <- fmap Right <$> holdDyn defCID aRespState'
  eRespState <- endpointRequest cidState (pure . pure $ "get-mixer-state")
    (pure . pure $ JSON.toJSON vals) (() <$ updated cidState)
  let eRespState' = catMaybes $ makeResponse <$> eRespState
  getStatus client 50 cidState eRespState'

getStatus :: forall a t m . (MonadWidget t m, FromJSON a) => ApiClient t m -> Integer -> DynReqBody t ContractInstanceId -> Event t () -> m (Event t a)
getStatus ApiClient{..} i d e = do
  performEvent_ $ logInfo (pack $ show i) <$ e
  performEvent_ $ fmap (logInfo . pack . show) e
  sResp <- statusRequest d e
  let sResp' = fmap ((fromJSONValue :: JSON.Value -> Maybe a) . observableState . cicCurrentState) $
        catMaybes $ makeResponse <$> sResp
      sRespError = () <$ ffilter isNothing (makeResponse <$> sResp)
  eRetry <- delay 10 $ leftmost [() <$ ffilter isNothing sResp', sRespError]
  sResp'' <- if i > 0 then getStatus (ApiClient{..}) (i-1) d eRetry else pure never
  return $ leftmost [catMaybes $ ffilter isJust sResp', sResp'']

fromJSONValue :: FromJSON b => JSON.Value -> Maybe b
fromJSONValue v = case fromJSON v of
  Success a -> Just a
  _         -> Nothing

toInputMap :: [Fr] -> WithdrawInputMap
toInputMap ins
  | length ins /= 32 = WithdrawInputMap "" "" "" "" "" "" "" "" "" [] [] "" "" ""
  | otherwise        = WithdrawInputMap root a h hA curPos oh nh r1 r2
      [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10] [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10] v1 v2 v3
  where [root, a, h, hA, curPos, oh, nh, r1, r2,
          o1, o2, o3, o4, o5, o6, o7, o8, o9, o10,
          l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, v1, v2, v3] = map (pack . show . fromZp) ins

newtype ProofJS = ProofJS { unProofJS :: Proof }
  deriving (Show, Generic)

instance FromJSON ProofJS where
  parseJSON (Object v) = do
      [a1t, a2t, _]                   <- v .: "pi_a"
      [[b11t, b12t], [b21t, b22t], _] <- v .: "pi_b"
      [c1t, c2t, _]                   <- v .: "pi_c"
      let [a1, a2, b11, b12, b21, b22, c1, c2] = map (toZp . read) [a1t, a2t, b11t, b12t, b21t, b22t, c1t, c2t]
      return $ ProofJS $ Proof (CP a1 a2) (CP (E (P [b11, b12])) (E (P [b21, b22]))) (CP c1 c2)
  parseJSON _ = pure $ ProofJS $ Proof O O O

data TxUnsignedCW = TxUnsignedCW { passphrase :: String, transaction :: Text }
    deriving (Generic, FromJSON, ToJSON)

getTxUnsignedCW :: Text -> Text
getTxUnsignedCW txt = transaction $ fromMaybe (TxUnsignedCW "" "") (decode $ fromStrict $ Data.Text.Encoding.encodeUtf8 txt :: Maybe TxUnsignedCW)

newtype TxSignedCW = TxSignedCW { unTxSignedCW :: Text }
    deriving Show

instance ToJSON TxSignedCW where
  toJSON (TxSignedCW tx) = JSON.object ["transaction" .= tx]

instance FromJSON TxSignedCW where
    parseJSON (JSON.Object v) = TxSignedCW <$> v .: "transaction"
    parseJSON _ = undefined
