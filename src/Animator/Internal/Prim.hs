
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances #-}

-- | Primitive imports form the host environment.
module Animator.Internal.Prim
(
JsString,
JsObject,
JsArray,

-- ** Objects
global,
new,
-- getString,
-- setString,
-- getInt,
-- setInt,
JsName,
JsProp(..),
JsElem(..),

-- ** JSON
HJ.JSON,

-- ** Utility
alert,
consoleLog,
documentWrite,
global,
)
where

import Data.String (IsString(..))
import qualified Haste.Prim as H
import qualified Haste.JSON as HJ




-- | Represents an unboxed JavaScript string.
newtype JsString = JsString { getJsString :: H.JSString }
deriving instance IsString JsString

toJsString :: String -> JsString  
toJsString = JsString . H.toJSStr 

fromJsString :: JsString -> String
fromJsString = H.fromJSStr . getJsString

-- | Represents an unboxed JavaScript object.
newtype JsObject = JsObject { getJsObject :: H.JSAny }

-- | Represents an unboxed JavaScript array.
newtype JsArray  = JsArray { getJsArray :: H.JSAny }








foreign import ccall "animator_global"        global#    :: IO H.JSAny
foreign import ccall "animator_object_create" new#       :: IO H.JSAny

foreign import ccall "animator_object_get"    getInt#    :: H.JSString -> H.JSAny -> IO Int
foreign import ccall "animator_object_get"    getString# :: H.JSString -> H.JSAny -> IO H.JSString
foreign import ccall "animator_object_get"    getObj#    :: H.JSString -> H.JSAny -> IO H.JSAny

foreign import ccall "animator_object_set"    setInt#    :: H.JSString -> H.JSAny -> Int -> IO ()
foreign import ccall "animator_object_set"    setString# :: H.JSString -> H.JSAny -> H.JSString -> IO ()
foreign import ccall "animator_object_set"    setObj#    :: H.JSString -> H.JSAny -> H.JSAny -> IO ()

-- | Returns the JavaScript global object.
global :: IO JsObject
global = global# >>= (return . JsObject)

-- | Creates a new JavaScript object. Equivalent to the literal value @{}@.
new :: IO JsObject
new = new# >>= (return . JsObject)

type JsName = String

getString :: JsName -> JsObject -> IO String
getString name obj = 
    getString# (H.toJSStr name) (getJsObject obj) >>= (return . H.fromJSStr)

setString :: JsName -> JsObject -> String -> IO ()
setString name obj value = 
    setString# (H.toJSStr name) (getJsObject obj) (H.toJSStr value)

getInt :: JsName -> JsObject -> IO Int
getInt name obj = 
    getInt# (H.toJSStr name) (getJsObject obj) >>= return

setInt :: JsName -> JsObject -> Int -> IO ()
setInt name obj value = 
    setInt# (H.toJSStr name) (getJsObject obj) value

getObj :: JsName -> JsObject -> IO JsObject
getObj name obj = 
    getObj# (H.toJSStr name) (getJsObject obj) >>= (return . JsObject)

setObj :: JsName -> JsObject -> JsObject -> IO ()
setObj name obj value = 
    setObj# (H.toJSStr name) (getJsObject obj) (getJsObject value)

-- | Class of types that can be properties of a JsObject.
class JsProp a where
    get     :: JsName -> JsObject -> IO a
    set     :: JsName -> JsObject -> a -> IO ()
    update  :: JsName -> JsObject -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

instance JsProp String where
    get = getString
    set = setString

instance JsProp Int where
    get = getInt
    set = setInt

instance JsProp JsObject where
    get = getObj
    set = setObj

-- | Class of types that can be elemnts in a JsArray.
class JsElem a where
    getE    :: Int -> JsArray -> IO a
    setE    :: Int -> JsArray -> a -> IO ()
    updateE :: Int -> JsArray -> (a -> a) -> IO ()
    updateE n o f = getE n o >>= setE n o . f

instance JsElem String where
    getE = undefined
    setE = undefined

instance JsElem Int where
    getE = undefined
    setE = undefined

instance JsElem JsObject where
    getE = undefined
    setE = undefined










foreign import ccall "animator_write" documentWrite#    :: H.JSString -> IO ()
foreign import ccall "animator_log"   consoleLog#       :: H.JSString -> IO ()
foreign import ccall "animator_alert" alert#            :: H.JSString -> IO ()

-- | Like @window.alert@ in JavaScript, i.e. displays a modal window with the given text.
alert :: String -> IO ()
alert str  = alert# (H.toJSStr $ str)

-- | Like @window.console.log@ in JavaScript, i.e. posts a line to the web console.
consoleLog :: String -> IO ()
consoleLog str = consoleLog# (H.toJSStr $ str)

-- | Like @window.document.write@ in JavaScript, i.e. appends the given content at the end of the `body` element.
documentWrite :: String -> IO ()
documentWrite str = documentWrite# (H.toJSStr $ "<code>" ++ str ++ "</code><br/>")
