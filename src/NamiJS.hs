{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module NamiJS where

-- TODO: use hidden inputs to pass values from fromises
-- cardano.nami.smth().then(
-- (val) => {ok}

import           Control.Monad.IO.Class          (MonadIO(..))
import           Data.Text                       (Text)
#ifdef __GHCJS__
import           Language.Javascript.JSaddle     (ToJSVal(..), JSVal)
#else
import           Language.Javascript.JSaddle     (JSVal)
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('isEnabled');\
    namiIsEnabled($1);\
  })();"
  isEnabled_js :: JSVal -> IO ()
#else
isEnabled_js :: JSVal -> IO ()
isEnabled_js = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
isEnabled :: MonadIO m => Text -> m ()
isEnabled txt = liftIO $ toJSVal txt >>= isEnabled_js
#else
isEnabled :: MonadIO m => Text -> m ()
isEnabled = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('enable');\
    namiEnable($1);\
  })();"
  enable_js :: JSVal -> IO ()
#else
enable_js :: JSVal -> IO ()
enable_js = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
enable :: MonadIO m => Text -> m ()
enable txt = liftIO $ toJSVal txt >>= enable_js
#else
enable :: MonadIO m => Text -> m ()
enable = const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('runDeposit');\
    runDeposit($1, $2, $3);\
  })();"
  runDeposit_js :: JSVal -> JSVal -> JSVal -> IO ()
#else
runDeposit_js :: JSVal -> JSVal -> JSVal -> IO ()
runDeposit_js = const . const . const $ error "GHCJS is required!"
#endif

#ifdef __GHCJS__
runDeposit :: MonadIO m => Text -> Text -> Text -> m ()
runDeposit elId elTx val = liftIO $ do
  elId_js <- toJSVal elId
  elTx_js <- toJSVal elTx
  val_js  <- toJSVal val
  runDeposit_js elId_js elTx_js val_js
#else
runDeposit :: MonadIO m => Text -> Text -> Text -> m ()
runDeposit = const . const . const $ error "GHCJS is required!"
#endif
