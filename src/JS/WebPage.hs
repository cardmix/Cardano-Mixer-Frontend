{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.WebPage where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
#endif

-----------------------------------------------------------------

-- WebFlow header script
#ifdef __GHCJS__
foreign import javascript unsafe
  "runHeadScripts();" runHeadScripts_js :: IO ()

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js
#else
runHeadScripts :: MonadIO m => m ()
runHeadScripts = error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "copyElemContent($1);" copyElemContent_js :: JSVal -> IO ()

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent txt = liftIO $ toJSVal txt >>= copyElemContent_js
#else
copyElemContent :: MonadIO m => Text -> m ()
copyElemContent _ = liftIO $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "saveTextFile($1);" saveTextFile_js :: JSVal -> IO ()

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile txt = liftIO $ toJSVal txt >>= saveTextFile_js
#else
saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);" logInfoJS :: JSVal -> IO ()

logInfo :: MonadIO m => Text -> m ()
logInfo txt = liftIO $ toJSVal txt >>= logInfoJS
#else
logInfo :: MonadIO m => Text -> m ()
logInfo = const $ error "GHCJS is required!"
#endif
