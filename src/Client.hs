{-# LANGUAGE DeriveAnyClass #-}
module Client where

import Data.Aeson as JSON
import Data.Proxy
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics
import Reflex.Dom hiding (Value)
import Servant.API
import Servant.Reflex

-- Types

newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey, ToHttpApiData)
    deriving anyclass (FromJSON, ToJSON)

data ContractActivationArgs t = ContractActivationArgs
  { caID     :: t
  , caWallet :: Maybe Wallet
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ContractState = ContractState
  { cicCurrentState :: PartiallyDecodedResponse }

instance FromJSON ContractState where
  parseJSON = withObject "ContractState" $ \v ->
    ContractState <$> v .: "cicCurrentState"

newtype PartiallyDecodedResponse = PartiallyDecodedResponse
  { observableState :: Value }

instance FromJSON PartiallyDecodedResponse where
  parseJSON = withObject "PartiallyDecodedResponse" $ \v ->
    PartiallyDecodedResponse <$> v .: "observableState"

newtype Wallet = Wallet { getWalletId :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- API

type API contractId
  = "api" :> "contract" :>
    (    "activate" :> ReqBody '[JSON] (ContractActivationArgs contractId)
                    :> Post '[JSON] ContractInstanceId
    :<|> "instance" :> Capture "contract-instance-id" ContractInstanceId :>
      "status" :> Get '[JSON] ContractState
    :<|> "instance" :> Capture "contract-instance-id" ContractInstanceId :>
      "endpoint" :> Capture "endpoint-name" String :> ReqBody '[JSON] JSON.Value :> Post '[JSON] ()
    :<|> "instance" :> Capture "contract-instance-id" ContractInstanceId :>
      "stop" :> Put '[JSON] ()
    )

type API' = API UUID

-- Client

type DynParam t a = Dynamic t (QParam a)
type DynReqBody t a = Dynamic t (Either Text a)
type RespEvent t a = Event t (ReqResult () a)
type Res t m res = Event t () -> m (RespEvent t res)
type ReqRes t m req res = DynReqBody t req -> Res t m res
type ReqRes3 t m req1 req2 req3 res = DynReqBody t req1 -> DynReqBody t req2 ->
  DynReqBody t req3 -> Res t m res

data ApiClient t m = ApiClient
  { activateRequest
    :: ReqRes t m (ContractActivationArgs UUID) ContractInstanceId
  , statusRequest :: ReqRes t m ContractInstanceId ContractState
  , endpointRequest :: ReqRes3 t m ContractInstanceId String JSON.Value ()
  , stopRequest :: ReqRes t m ContractInstanceId ()
  }

mkApiClient :: forall t m . MonadWidget t m =>
  BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (activateRequest :<|> statusRequest :<|> endpointRequest :<|> stopRequest)
      = client (Proxy @API') (Proxy @m) (Proxy @()) (pure host)

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing
