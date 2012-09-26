
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-- | Primitive imports form the Host environment.
module Animator.Internal.Prim
(
JsString,
JsObject,
JsArray,
HJ.JSON,
alert,
consoleLog,
documentWrite,
)
where

import qualified Haste.Prim as H
import qualified Haste.JSON as HJ
import Data.String

newtype JsString = JsString { getJSString :: H.JSString }
deriving instance IsString JsString

toJsString :: String -> JsString  
toJsString = undefined

fromJsString :: JsString -> String
fromJsString = undefined



newtype JsObject = JsObject { getJSObject :: H.JSAny }
newtype JsArray  = JsArray { getJSArray :: H.JSAny }



foreign import ccall "animator_write" animator_write :: H.JSString -> IO ()
foreign import ccall "animator_log" animator_log     :: H.JSString -> IO ()
foreign import ccall "animator_alert" animator_alert :: H.JSString -> IO ()
alert str  = animator_alert (H.toJSStr $ str)
consoleLog str = animator_log (H.toJSStr $ str)
documentWrite str = animator_write (H.toJSStr $ "<code>" ++ str ++ "</code><br/>")
