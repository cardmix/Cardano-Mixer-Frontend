{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveAnyClass #-}

module JS where

import Control.Monad.IO.Class
import Data.Text
import GHC.Generics

import Language.Javascript.JSaddle

foreign import javascript unsafe
  "runHeadScripts();" runHeadScripts_js :: IO ()

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js

foreign import javascript unsafe
  "copyElemContent($1);" copyElemContent_js :: Text -> IO ()

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent = liftIO . copyElemContent_js

foreign import javascript unsafe
  "saveTextFile($1);" saveTextFile_js :: Text -> IO ()

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = liftIO . saveTextFile_js

foreign import javascript unsafe
  "autofillAddr($1);" autofillAddr_js :: Text -> IO ()

autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = liftIO . autofillAddr_js

foreign import javascript unsafe
  "fillProof($1, $2);" fillProof_js :: Text -> JSVal -> IO ()

fillProof :: MonadIO m => Text -> WithdrawInputMap -> m ()
fillProof elId wIM  = liftIO $ toJSVal wIM >>= fillProof_js elId

foreign import javascript unsafe
  "console.log($1);" logInfoJS :: Text -> IO ()

logInfo :: MonadIO m => Text -> m ()
logInfo = liftIO . logInfoJS

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