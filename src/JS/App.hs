{-# LANGUAGE CPP                 #-}
{-# LANGUAGE JavaScriptFFI       #-}

{-# HLINT ignore "Use camelCase" #-}

module JS.App where

import           Control.Monad.IO.Class      (MonadIO(..))
import           Data.Text                   (Text)

#ifdef __GHCJS__
import           Language.Javascript.JSaddle (ToJSVal(..), JSVal)
#endif

import           JS.Types

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('isEnabled');\
    namiIsEnabled($1);\
  })();"
  isEnabled_js :: JSVal -> IO ()

isEnabled :: MonadIO m => Text -> m ()
isEnabled txt = liftIO $ toJSVal txt >>= isEnabled_js
#else
isEnabled :: MonadIO m => Text -> m ()
isEnabled = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('enable');\
    namiEnable($1);\
  })();"
  enable_js :: JSVal -> IO ()

enable :: MonadIO m => Text -> m ()
enable txt = liftIO $ toJSVal txt >>= enable_js
#else
enable :: MonadIO m => Text -> m ()
enable = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "autofillAddr($1);" autofillAddr_js :: JSVal -> IO ()

autofillAddr :: MonadIO m => Text -> m ()
autofillAddr txt = liftIO $ toJSVal txt >>= autofillAddr_js
#else
autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "(function() {\
    console.log('runDeposit');\
    runDeposit($1, $2, $3);\
  })();"
  runDeposit_js :: JSVal -> JSVal -> JSVal -> IO ()

runDeposit :: MonadIO m => Text -> Text -> DepositParams -> m ()
runDeposit elId elTx dp = liftIO $ do
  elId_js <- toJSVal elId
  elTx_js <- toJSVal elTx
  dp_js   <- toJSVal dp
  runDeposit_js elId_js elTx_js dp_js
#else
runDeposit :: MonadIO m => Text -> Text -> DepositParams -> m ()
runDeposit = const . const . const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "fillProof($1, $2);" fillProof_js :: JSVal -> JSVal -> IO ()

fillProof :: MonadIO m => Text -> Text -> m ()
fillProof elId wIM  = liftIO $ do
  wIM_js <- toJSVal wIM
  elId_js <- toJSVal elId
  fillProof_js elId_js wIM_js
#else
fillProof :: MonadIO m => Text -> Text -> m ()
fillProof _ _ = liftIO $ error "GHCJS is required!"
#endif
