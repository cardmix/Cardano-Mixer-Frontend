{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
module NamiJS where

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
  })();"
  enable_js :: IO ()

enable :: MonadIO m => m ()
enable = liftIO enable_js
