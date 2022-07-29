module Client where

import           Data.Aeson                as JSON
import           Data.Proxy                (Proxy(..))
import           Data.Text                 (Text)
import           Reflex.Dom                hiding (Value)
import           Servant.API
import           Servant.Reflex

import           MixerProofs.SigmaProtocol

-- API

type API
  = "api" :>
    (
         "keys"                                   :> Get  '[JSON] KeysResponse
    :<|> "withdraw" :> ReqBody '[JSON] WithdrawRequest :> Post '[JSON] ()
    )

-- Client

type RespEvent t a      = Event t (ReqResult () a)
type Res t m res        = Event t () -> m (RespEvent t res)
type DynReqBody t a     = Dynamic t (Either Text a)
type ReqRes t m req res = DynReqBody t req -> Res t m res

data ApiClient t m = ApiClient
  {
    keysRequest     :: Res t m KeysResponse,
    withdrawRequest :: ReqRes t m WithdrawRequest ()
  }

mkApiClient :: forall t m . MonadWidget t m =>
  BaseUrl -> ApiClient t m
mkApiClient host = ApiClient{..}
  where
    (keysRequest :<|> withdrawRequest) = client (Proxy @API) (Proxy @m) (Proxy @()) (pure host)

makeResponse :: ReqResult tag a -> Maybe a
makeResponse (ResponseSuccess _ a _) = Just a
makeResponse _ = Nothing
