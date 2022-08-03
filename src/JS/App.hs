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
  "walletEnable($1, $2);" enable_js :: JSVal -> JSVal -> IO ()

enable :: MonadIO m => Text -> Text -> m ()
enable walletName resId = liftIO $ do
  walletName_js <- toJSVal walletName
  resId_js      <- toJSVal resId
  enable_js walletName_js resId_js
#else
enable :: MonadIO m => Text -> Text -> m ()
enable = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "walletAddress($1, $2);" walletAddress_js :: JSVal -> JSVal -> IO ()

walletAddress :: MonadIO m => Text -> Text -> m ()
walletAddress walletName resId = liftIO $ do
  walletName_js <- toJSVal walletName
  resId_js      <- toJSVal resId
  walletAddress_js walletName_js resId_js
#else
walletAddress :: MonadIO m => Text -> Text -> m ()
walletAddress = const $ error "GHCJS is required!"
#endif

-----------------------------------------------------------------

#ifdef __GHCJS__
foreign import javascript unsafe
  "runDeposit($1, $2, $3);"
  runDeposit_js :: JSVal -> JSVal -> JSVal -> IO ()

runDeposit :: MonadIO m => Text -> DepositParams -> Text -> m ()
runDeposit walletName dp resId = liftIO $ do
  walletName_js <- toJSVal walletName
  dp_js   <- toJSVal dp
  resId_js      <- toJSVal resId
  runDeposit_js walletName_js dp_js resId_js
#else
runDeposit :: MonadIO m => Text -> DepositParams -> Text -> m ()
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
