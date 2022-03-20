module Backend where

import Client
import Data.Aeson as JSON
import Data.Text (Text)
import Data.UUID as UUID
import Data.Witherable
import JS
import NamiJS as Nami
import Reflex.Dom hiding (Value)
import Servant.Reflex

data Token = TokenADA | TokenTest deriving (Eq, Show, Ord, Bounded, Enum)

runDeposit :: MonadWidget t m => Text -> Integer -> Dynamic t (Token, Int) ->
  Event t () -> m ()
runDeposit elId key dReq eDeposit = do
  let ApiClient{..} = mkApiClient pabIP
  -- отправляет запрос к бэкенду, который возвращает String ???
  -- endpointRequest pabIP "deposit" cidMixerUse (DepositParams pkh mixVal leaf)
  eResp <- endpointRequest (pure . pure $ contractInstanceId)
    (pure . pure $ "deposit") (mkDepositParams key <$> dReq) eDeposit
  dAddr <- fmap value $ inputElement $ def
    & initialAttributes .~ ("style" =: "display:none;" <> "id" =: elemId)
  performEvent_ (autofillAddr elemId <$ eDeposit)
  let eStr = "string" <$ catMaybes (makeResponse <$> eResp)
  -- Этот String отправляем в balanceTx -> signTx -> submitTx (это все функции API кошелька).
  performEvent_ (Nami.runDeposit elId <$> eStr)
  where
    elemId = "deposit-addr-elem"
    pabIP = BasePath "TODO"
    contractInstanceId = ContractInstanceId UUID.nil -- ???
    mkDepositParams key (token, amount) = Right JSON.Null -- ???

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Int, Text)))
findDeposit _dAddr _dKey e = (Just (1, "ADA") <$) <$> delay 1 e
-- Отправлять какой-нибудь API запрос к бэкенду (заглушка)

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw _dAddr _dKey e = (True <$) <$> delay 2 e
-- Отправлять какой-нибудь API запрос к бэкенду (заглушка)
