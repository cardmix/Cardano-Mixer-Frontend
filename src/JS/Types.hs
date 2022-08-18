{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.Types where

import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

import           Language.Javascript.JSaddle (ToJSVal(..))

data DepositParams = DepositParams
  {
    dpAddress      :: Text,
    dpADAValue     :: Text,
    dpNonADAValue  :: [(Text, Text, Text)],  -- [(symbol, name, quantity)]
    dpKey          :: (Text, Text)           -- (key, secret)
  }
  deriving (Generic, ToJSVal)

data WithdrawInputMap = WithdrawInputMap
  {
    imRoot :: Text,
    imAddr :: Text,
    imH    :: Text,
    imHA   :: Text,
    imCurPos :: Text,
    imOldHashReward :: Text,
    imNewHashReward :: Text,
    imR1 :: Text,
    imR2 :: Text,
    imO :: [Text],
    imL :: [Text],
    imV1 :: Text,
    imV2 :: Text,
    imV3 :: Text
  }
  deriving (Generic, ToJSVal)
