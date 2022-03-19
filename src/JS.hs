{-# LANGUAGE CPP           #-}
{-# LANGUAGE JavaScriptFFI #-}
module JS where

import Control.Monad.IO.Class
import Data.Text

foreign import javascript unsafe
  "WebFont.load(\
    {google:{families:\
      ['Droid Serif:400,400italic,700,700italic',\
      'Corben:regular','Fenix:regular']}});\
  !function(o,c) {\
    var n=c.documentElement, t=' w-mod-';\
    n.className+=t+'js';\
    if ('ontouchstart'in o||o.DocumentTouch && c instanceof DocumentTouch) {\
      n.className+=t+'touch';\
    }\
  }(window,document);"
  runHeadScripts_js :: IO ()

runHeadScripts :: MonadIO m => m ()
runHeadScripts = liftIO runHeadScripts_js

foreign import javascript unsafe
  "(function() {\
    var el = document.getElementById($1);\
    if (el != null && navigator && navigator.clipboard && navigator.clipboard.writeText) {\
      navigator.clipboard.writeText(el.innerText);\
    }\
  })();"
  copyElemContent_js :: Text -> IO ()

copyElemContent :: MonadIO m => Text -> m ()
copyElemContent = liftIO . copyElemContent_js

foreign import javascript unsafe
  "(function() {\
    var element = document.createElement('a');\
      element.setAttribute('href', 'data:application/octet-stream,' +\
        encodeURIComponent($1));\
      element.setAttribute('download', 'key.txt');\
      element.style.display = 'none';\
      document.body.appendChild(element);\
      element.click();\
      document.body.removeChild(element);\
  })();"
  saveTextFile_js :: Text -> IO ()

saveTextFile :: MonadIO m => Text -> m ()
saveTextFile = liftIO . saveTextFile_js

foreign import javascript unsafe
  "(function() {\
    var val = 'HELLO';\
    var el = document.getElementById($1);\
    if (el != null) {\
      el.value = val;\
      var eChange = new Event('change');\
      var eInput = new Event('input');\
      el.dispatchEvent(eChange);\
      el.dispatchEvent(eInput);\
    };\
  })();"
  autofillAddr_js :: Text -> IO ()

autofillAddr :: MonadIO m => Text -> m ()
autofillAddr = liftIO . autofillAddr_js

