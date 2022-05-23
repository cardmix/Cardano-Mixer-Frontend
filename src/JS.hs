{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveAnyClass #-}

module JS where

import Control.Monad.IO.Class
import Data.Text
import GHC.Generics (Generic)
import Language.Javascript.JSaddle

#ifdef __GHCJS__
foreign import javascript unsafe
  "runHeadScripts();" runHeadScripts_js :: IO ()
#else
runHeadScripts_js :: IO ()
runHeadScripts_js = error "GHCJS is required!"
#endif

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "copyElemContent($1);" copyElemContent_js :: Text -> IO ()
#else
copyElemContent_js :: Text -> IO ()
copyElemContent_js = const $ error "GHCJS is required!"
#endif

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent = liftIO . copyElemContent_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveTextFile($1);" saveTextFile_js :: Text -> IO ()
#else
saveTextFile_js :: Text -> IO ()
saveTextFile_js = const $ error "GHCJS is required!"
#endif

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = liftIO . saveTextFile_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "autofillAddr($1);" autofillAddr_js :: Text -> IO ()
#else
autofillAddr_js :: Text -> IO ()
autofillAddr_js = const $ error "GHCJS is required!"
#endif

autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = liftIO . autofillAddr_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "fillProof($1, $2);" fillProof_js :: Text -> JSVal -> IO ()
#else
fillProof_js :: Text -> JSVal -> IO ()
fillProof_js = const . const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
fillProof :: MonadIO m => Text -> WithdrawInputMap -> m ()
fillProof elId wIM  = liftIO $ toJSVal wIM >>= fillProof_js elId
#else
fillProof :: MonadIO m => Text -> WithdrawInputMap -> m ()
fillProof _ _ = liftIO $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);" logInfoJS :: Text -> IO ()
#else
logInfoJS :: Text -> IO ()
logInfoJS = const $ error "GHCJS is required!"
#endif

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
