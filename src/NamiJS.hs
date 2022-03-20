{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
module NamiJS where

-- TODO: use hidden inputs to pass values from fromises
-- cardano.nami.smth().then(
-- (val) => {ok}

import Control.Monad.IO.Class

foreign import javascript unsafe
  "(function() {\
    console.log('isEnabled');\
    return false;\
  })();"
  isEnabled_js :: IO Bool

isEnabled :: MonadIO m => m Bool
isEnabled = liftIO isEnabled_js

foreign import javascript unsafe
  "(function() {\
    console.log('enable');\
    namiEnable('');\
  })();"
  enable_js :: IO ()

enable :: MonadIO m => m ()
enable = liftIO enable_js

