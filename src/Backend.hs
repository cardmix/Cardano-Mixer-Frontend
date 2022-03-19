module Backend where

import Data.Text (Text)
import Reflex.Dom hiding (Value)

runDeposit :: MonadWidget t m => Integer -> Event t () -> m (Event t ())
runDeposit _ e = delay 1 e
-- отправляет запрос к бэкенду, который возвращает String.
-- Этот String отправляем в balanceTx -> signTx -> submitTx (это все функции API кошелька).

findDeposit :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t (Maybe (Int, Text)))
findDeposit _dAddr _dKey e = (Just (1, "ADA") <$) <$> delay 1 e
-- Отправлять какой-нибудь API запрос к бэкенду (заглушка)

runWithdraw :: MonadWidget t m => Dynamic t Text -> Dynamic t Text
  -> Event t () -> m (Event t Bool)
runWithdraw _dAddr _dKey e = (True <$) <$> delay 2 e
-- Отправлять какой-нибудь API запрос к бэкенду (заглушка)
