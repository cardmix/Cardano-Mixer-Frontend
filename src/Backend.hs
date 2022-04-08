{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}

module Backend where

import Client
import Data.Aeson hiding (Value)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import Data.UUID as UUID
import Data.Witherable
import JS
import NamiJS as Nami
import Reflex.Dom hiding (Value)
import Servant.Reflex

import Control.Monad.IO.Class
import GHC.Generics
import MixerUserData
import Data.Maybe (fromJust)
import Crypto
import MixerFrontendContractParams
import Data.Tuple.Extra
import qualified Data.Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.ByteString hiding (drop, pack, unpack, length, map)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (pack, unpack)
import Data.Text.Encoding
import MixerState
import MixerProofs
import Data.Tuple.Select

data Token = TokenADA | TokenTest deriving (Eq, Show, Ord, Bounded, Enum)

toValue :: Token -> Integer -> Value
toValue TokenADA  k = Value $ Data.Map.singleton (CurrencySymbol "") (Data.Map.singleton (TokenName "") (k*2_000))
toValue TokenTest k = Value $ Data.Map.singleton (CurrencySymbol "975105") (Data.Map.singleton (TokenName "81239") k)

runDeposit :: MonadWidget t m => Text -> DepositSecret -> Dynamic t (Token, Integer) ->
  Event t () -> m ()
runDeposit elId key dReq eDeposit = do
  let client@ApiClient{..} = mkApiClient pabIP

  -- Update wallet address
  performEvent_ (autofillAddr elemId <$ eDeposit)
  dAddr <- fmap value $ inputElement $ def
    & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId) -- ("class" =: "textform" <> "id" =: elemId)

  -- Connect to PAB
  aRespConnect <- activateRequest (pure . pure $ ContractActivationArgs (FrontendContracts ConnectToPAB) Nothing) (fmap (\_ -> ()) $ updated dAddr)
  let aRespConnect' = catMaybes $ makeResponse <$> aRespConnect
  cidPAB <- fmap Right <$> holdDyn contractInstanceId aRespConnect'
  eRespConnect <- endpointRequest cidPAB (pure . pure $ "Connect to PAB")
    (fmap (Right . String) dAddr)
    (fmap (\_ -> ()) $ updated cidPAB)
  let eRespConnect' = catMaybes $ makeResponse <$> eRespConnect
  sRespConnect <- statusRequest cidPAB eRespConnect'
  let sRespConnect' = catMaybes $ fmap ((fromJSONValue :: JSON.Value -> Maybe (PaymentPubKeyHash, Fr, Wallet)) . observableState . cicCurrentState) $
                  catMaybes $ makeResponse <$> sRespConnect
  dRespConnect <- holdDyn (PaymentPubKeyHash $ PubKeyHash "", Zp 0, Wallet "") sRespConnect'
  let dPABWallet = fmap (Just . thd3) dRespConnect

  -- Deposit
  aRespDeposit <- activateRequest (fmap Right (pure (ContractActivationArgs (FrontendContracts MixerUse)) <*> dPABWallet))
    (fmap (\_ -> ()) $ updated dRespConnect)
  let aRespDeposit' = catMaybes $ makeResponse <$> aRespDeposit
  cidMixerUse <- fmap Right <$> holdDyn contractInstanceId aRespDeposit'
  eRespDeposit <- endpointRequest cidMixerUse
    (pure . pure $ "deposit") (mkDepositParams key <$> dAddr <*> dReq)
    (fmap (\_ -> ()) $ updated cidMixerUse)
  let eRespDeposit' = catMaybes $ makeResponse <$> eRespDeposit
  sRespDeposit' <- getStatus client 10 cidMixerUse eRespDeposit'  
  performEvent_ (Nami.runDeposit elId elemIdTx . getTxUnsignedCW <$> sRespDeposit')

  dTx <- fmap value $ inputElement $ def
    & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemIdTx)

  -- Deposit submit
  aRespReturn <- activateRequest (fmap Right (pure (ContractActivationArgs (FrontendContracts MixerUse)) <*> dPABWallet)) (() <$ updated dTx)
  let aRespReturn' = catMaybes $ makeResponse <$> aRespReturn
  cidMixerReturn <- fmap Right <$> holdDyn contractInstanceId aRespReturn'
  _ <- endpointRequest cidMixerReturn (pure . pure $ "depositSubmit") (Right . JSON.toJSON <$> dTx) (() <$ updated cidMixerReturn)

  pure ()
  where
    elemId = "deposit-addr-elem"
    elemIdTx = "tx-signed-elem"
    pabIP = BasePath "http://18.185.86.35:9080"
    contractInstanceId = ContractInstanceId UUID.nil -- ???
    mkDepositParams key addr (token, amount) = Right $ JSON.toJSON $ DepositParams addr (toValue token amount) (mimcHash (getR1 key) (getR2 key))

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Int, Text)))
findDeposit dAddr dKey e = do
  (Just (1, "ADA") <$) <$> delay 1 e

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw dAddr dKey e = do
  let client@ApiClient{..} = mkApiClient pabIP

  -- randomness :: ZKProofSecret <- liftIO $ generateProofSecret

  -- Connect to PAB
  aRespConnect <- activateRequest (pure . pure $ ContractActivationArgs (FrontendContracts ConnectToPAB) Nothing) e
  let aRespConnect' = catMaybes $ makeResponse <$> aRespConnect
  cidPAB <- fmap Right <$> holdDyn contractInstanceId aRespConnect'
  eRespConnect <- endpointRequest cidPAB (pure . pure $ "Connect to PAB")
    (fmap (Right . String) dAddr)
    (fmap (\_ -> ()) $ updated cidPAB)
  let eRespConnect' = catMaybes $ makeResponse <$> eRespConnect
  sRespConnect <- statusRequest cidPAB eRespConnect'
  let sRespConnect' = catMaybes $ fmap ((fromJSONValue :: JSON.Value -> Maybe (PaymentPubKeyHash, Fr, Wallet)) . observableState . cicCurrentState) $
                  catMaybes $ makeResponse <$> sRespConnect
  dRespConnect <- holdDyn (PaymentPubKeyHash $ PubKeyHash "", Zp 0, Wallet "") sRespConnect'
  let dPABWallet = fmap (Just . thd3) dRespConnect
      -- dPKH       = fmap fst3 dRespConnect
      dAddrNum   = fmap snd3 dRespConnect
  let dSecret = fmap readDepositSecret dKey

  -- Get MixerState
  aRespState <- activateRequest (pure . pure $ ContractActivationArgs (FrontendContracts MixerStateQuery) Nothing) (fmap (\_ -> ()) $ updated dRespConnect)
  let aRespState' = catMaybes $ makeResponse <$> aRespState
  cidState <- fmap Right <$> holdDyn contractInstanceId aRespState'
  eRespState <- endpointRequest cidState (pure . pure $ "Get Mixer state")
    (pure . pure $ JSON.toJSON val)
    (fmap (\_ -> ()) $ updated cidState)
  let eRespState' = catMaybes $ makeResponse <$> eRespState
  sRespState' <- getStatus client 10 cidState eRespState'
  dMixerState <- holdDyn [] sRespState'

  let dWires    = mkProveArgs <$> dAddrNum <*> dSecret <*> dMixerState
      dLastDeposit = fmap sel1 dWires
      dOuts        = fmap sel2 dWires
      dPubIns      = fmap sel3 dWires
      dPrivIns     = fmap sel4 dWires
      dSubsIns     = zipDynWith (++) dPubIns dPrivIns

  -- creating input map
  let dSubsMap    = fmap toInputMap dSubsIns
      eProveStart = attachPromptlyDynWith (\i s -> i) dSubsMap $ updated dMixerState
  
  performEvent_ (fillProof elemId <$> eProveStart)
  dProofText <- fmap value $ inputElement $ def
    & initialAttributes .~ ("class" =: "textform" <> "id" =: elemId)
  let dProof = fmap (unProofJS . fromMaybe (ProofJS $ Proof O O O) . decode . fromStrict . encodeUtf8) dProofText
      dParams = WithdrawParams val <$> dLastDeposit <*> dAddr <*> dPubIns <*> dProof

  aRespWithdraw <- activateRequest (fmap Right (pure (ContractActivationArgs (FrontendContracts MixerUse)) <*> dPABWallet)) (() <$ updated dProof)
  let aRespWithdraw' = catMaybes $ makeResponse <$> aRespWithdraw
  cidMixerWithdraw <- fmap Right <$> holdDyn contractInstanceId aRespWithdraw'
  _ <- endpointRequest cidMixerWithdraw (pure . pure $ "withdraw") (Right . JSON.toJSON <$> dParams) (() <$ updated cidMixerWithdraw)

  (True <$) <$> delay 2 e
  where
    elemId = "proof-elem"
    pabIP = BasePath "http://18.185.86.35:9080"
    contractInstanceId = ContractInstanceId UUID.nil -- ???
    val = toValue TokenADA 20
    testSAS = ShieldedAccountSecret (toZp 48081304776541705762740047315054676823791585112593769402696472791053032063395)
        (toZp 23242094360494712196167729465930882794461214075280956494392702130165068291126)
        (toZp 5355364785739370808595236295435031456604123972461255344590972311627062382674)
    mkProveArgs a ds state = computeWithdrawWires a ds testSAS state

------------------------------------------------------------------

getStatus :: forall a t m . (MonadWidget t m, FromJSON a) => ApiClient t m -> Integer -> DynReqBody t ContractInstanceId -> Event t () -> m (Event t a)
getStatus (ApiClient{..}) i d e = do
  -- rec
  sResp <- statusRequest d e
  let sResp' = fmap ((fromJSONValue :: JSON.Value -> Maybe a) . observableState . cicCurrentState) $
        catMaybes $ makeResponse <$> sResp
  eRetry <- delay 3 $ () <$ ffilter isNothing sResp'
  sResp'' <- if i > 0 then getStatus (ApiClient{..}) (i-1) d eRetry else pure never
  return $ catMaybes $ leftmost [sResp', sResp'']

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
      [a1t, a2t, a3t]                   <- v .: "pi_a"
      [[b11t, b12t], [b21t, b22t], b3t] <- v .: "pi_b"
      [c1t, c2t, c3t]                   <- v .: "pi_c"
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