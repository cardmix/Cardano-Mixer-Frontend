{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
module NamiJS where

-- TODO: use hidden inputs to pass values from fromises
-- cardano.nami.smth().then(
-- (val) => {ok}

import Control.Monad.IO.Class
import Data.Text

foreign import javascript unsafe
  "(function() {\
    console.log('isEnabled');\
    namiIsEnabled($1);\
  })();"
  isEnabled_js :: Text -> IO ()

isEnabled :: MonadIO m => Text -> m ()
isEnabled = liftIO . isEnabled_js

foreign import javascript unsafe
  "(function() {\
    console.log('enable');\
    namiEnable($1);\
  })();"
  enable_js :: Text -> IO ()

enable :: MonadIO m => Text -> m ()
enable = liftIO . enable_js

