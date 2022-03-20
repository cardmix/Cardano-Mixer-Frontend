{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
module JS where

import Control.Monad.IO.Class
import Data.Text

foreign import javascript unsafe
  "runHeadScripts();" runHeadScripts_js :: IO ()

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js

foreign import javascript unsafe
  "copyElemContent($1);" copyElemContent_js :: Text -> IO ()

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent = liftIO . copyElemContent_js

foreign import javascript unsafe
  "saveTextFile($1);" saveTextFile_js :: Text -> IO ()

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = liftIO . saveTextFile_js

foreign import javascript unsafe
  "autofillAddr($1);" autofillAddr_js :: Text -> IO ()

autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = liftIO . autofillAddr_js
