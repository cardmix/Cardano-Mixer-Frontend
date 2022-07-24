{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)

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
  "copyElemContent($1);" copyElemContent_js :: JSVal -> IO ()
#else
copyElemContent_js :: JSVal -> IO ()
copyElemContent_js = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
copyElemContent :: MonadIO m => Text -> m ()
copyElemContent txt = liftIO $ toJSVal txt >>= copyElemContent_js
#else
copyElemContent :: MonadIO m => Text -> m ()
copyElemContent _ = liftIO $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveTextFile($1);" saveTextFile_js :: JSVal -> IO ()
#else
saveTextFile_js :: JSVal -> IO ()
saveTextFile_js = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
saveTextFile :: MonadIO m => Text -> m ()
saveTextFile txt = liftIO $ toJSVal txt >>= saveTextFile_js
#else
saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "autofillAddr($1);" autofillAddr_js :: JSVal -> IO ()
#else
autofillAddr_js :: JSVal -> IO ()
autofillAddr_js = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
autofillAddr :: MonadIO m => Text -> m ()
autofillAddr txt = liftIO $ toJSVal txt >>= autofillAddr_js
#else
autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "fillProof($1, $2);" fillProof_js :: JSVal -> JSVal -> IO ()
#else
fillProof_js :: JSVal -> JSVal -> IO ()
fillProof_js = const . const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
fillProof :: MonadIO m => Text -> WithdrawInputMap -> m ()
fillProof elId wIM  = liftIO $ do
  wIM_js <- toJSVal wIM
  elId_js <- toJSVal elId
  fillProof_js elId_js wIM_js
#else
fillProof :: MonadIO m => Text -> WithdrawInputMap -> m ()
fillProof _ _ = liftIO $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);" logInfoJS :: JSVal -> IO ()
#else
logInfoJS :: JSVal -> IO ()
logInfoJS = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
logInfo :: MonadIO m => Text -> m ()
logInfo txt = liftIO $ toJSVal txt >>= logInfoJS
#else
logInfo :: MonadIO m => Text -> m ()
logInfo = const $ error "GHCJS is required!"
#endif

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
