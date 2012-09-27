
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances #-}

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
new,
getStr,
setStr,
getInt,
setInt,
JsName,
JsProperty(..)
)
where

import qualified Haste.Prim as H
import qualified Haste.JSON as HJ
import Data.String

newtype JsString = JsString { getJSString :: H.JSString }
deriving instance IsString JsString

toJsString :: String -> JsString  
toJsString = JsString . H.toJSStr 

-- fromJsString :: JsString -> String
-- fromJsString = H.fromJSStr . getJsString



newtype JsObject = JsObject { getJsObject :: H.JSAny }
newtype JsArray  = JsArray { getJsArray :: H.JSAny }


-- function animator_object_create(obj, _world) {
-- function animator_object_get(obj, name, _world) {
-- function animator_object_set(obj, name, value, _world) {

foreign import ccall "animator_object_create" 
    new' :: IO H.JSAny

foreign import ccall "animator_object_get"    
    getStr'    :: H.JSString -> H.JSAny -> IO H.JSString
foreign import ccall "animator_object_set"    
    setStr'    :: H.JSString -> H.JSAny -> H.JSString -> IO ()
foreign import ccall "animator_object_get"    
    getInt'    :: H.JSString -> H.JSAny -> IO Int
foreign import ccall "animator_object_set"    
    setInt'    :: H.JSString -> H.JSAny -> Int -> IO ()
foreign import ccall "animator_object_get"    
    getObj'    :: H.JSString -> H.JSAny -> IO H.JSAny
foreign import ccall "animator_object_set"    
    setObj'    :: H.JSString -> H.JSAny -> H.JSAny -> IO ()


new :: IO JsObject
new = new' >>= (return . JsObject)

type JsName = String

getStr :: JsName -> JsObject -> IO String
getStr name obj = 
    getStr' (H.toJSStr name) (getJsObject obj) >>= (return . H.fromJSStr)

setStr :: JsName -> JsObject -> String -> IO ()
setStr name obj value = 
    setStr' (H.toJSStr name) (getJsObject obj) (H.toJSStr value)

getInt :: JsName -> JsObject -> IO Int
getInt name obj = 
    getInt' (H.toJSStr name) (getJsObject obj) >>= return

setInt :: JsName -> JsObject -> Int -> IO ()
setInt name obj value = 
    setInt' (H.toJSStr name) (getJsObject obj) value

getObj :: JsName -> JsObject -> IO JsObject
getObj name obj = 
    getObj' (H.toJSStr name) (getJsObject obj) >>= (return . JsObject)

setObj :: JsName -> JsObject -> JsObject -> IO ()
setObj name obj value = 
    setObj' (H.toJSStr name) (getJsObject obj) (getJsObject value)

class JsProperty a where
    get :: JsName -> JsObject -> IO a
    set :: JsName -> JsObject -> a -> IO ()
    update :: JsName -> JsObject -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

instance JsProperty String where
    get = getStr
    set = setStr

instance JsProperty Int where
    get = getInt
    set = setInt

instance JsProperty JsObject where
    get = getObj
    set = setObj

foreign import ccall "animator_write" animator_write :: H.JSString -> IO ()
foreign import ccall "animator_log" animator_log     :: H.JSString -> IO ()
foreign import ccall "animator_alert" animator_alert :: H.JSString -> IO ()
alert str  = animator_alert (H.toJSStr $ str)

consoleLog :: String -> IO ()
consoleLog str = animator_log (H.toJSStr $ str)

documentWrite :: String -> IO ()
documentWrite str = animator_write (H.toJSStr $ "<code>" ++ str ++ "</code><br/>")
