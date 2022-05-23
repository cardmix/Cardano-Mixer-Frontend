{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module NamiJS where

-- TODO: use hidden inputs to pass values from fromises
-- cardano.nami.smth().then(
-- (val) => {ok}

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Text              (Text)

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('isEnabled');\
    namiIsEnabled($1);\
  })();"
  isEnabled_js :: Text -> IO ()
#else
isEnabled_js :: Text -> IO ()
isEnabled_js = const $ error "GHCJS is required!"
#endif

isEnabled :: MonadIO m => Text -> m ()
isEnabled = liftIO . isEnabled_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('enable');\
    namiEnable($1);\
  })();"
  enable_js :: Text -> IO ()
#else
enable_js :: Text -> IO ()
enable_js = const $ error "GHCJS is required!"
#endif

enable :: MonadIO m => Text -> m ()
enable = liftIO . enable_js

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('runDeposit');\
    runDeposit($1, $2, $3);\
  })();"
  runDeposit_js :: Text -> Text -> Text -> IO ()
#else
runDeposit_js :: Text -> Text -> Text -> IO ()
runDeposit_js = const . const . const $ error "GHCJS is required!"
#endif

runDeposit :: MonadIO m => Text -> Text -> Text -> m ()
runDeposit elId elTx val = liftIO $ runDeposit_js elId elTx val
