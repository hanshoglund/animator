
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances, ForeignFunctionInterface #-}

-------------------------------------------------------------------------------------
-- | 
-- Primitive imports form the host environment.
-------------------------------------------------------------------------------------

-- TODO Remove TypeSynonymInstances, FlexibleInstances?
--      Required for String instance of JsProp, can we rethink this?

module Animator.Internal.Prim (
        JsString,
        JsObject,
        JsArray,

        -- ** Objects
        global,
        new,
        JsName,
        JsProp(..),
        JsElem(..),

        -- ** JSON
        HJ.JSON,

        -- ** Utility
        windowAlert,
        windowConsoleLog,
        windowDocumentWrite,
  ) where

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







foreign import ccall "aPrimGlobal" global#    :: IO H.JSAny
foreign import ccall "aPrimObj"    new#       :: IO H.JSAny

foreign import ccall "aPrimGet"    getInt#    :: H.JSString -> H.JSAny -> IO Int
foreign import ccall "aPrimGet"    getString# :: H.JSString -> H.JSAny -> IO H.JSString
foreign import ccall "aPrimGet"    getObj#    :: H.JSString -> H.JSAny -> IO H.JSAny

foreign import ccall "aPrimSet"    setInt#    :: H.JSString -> H.JSAny -> Int -> IO ()
foreign import ccall "aPrimSet"    setString# :: H.JSString -> H.JSAny -> H.JSString -> IO ()
foreign import ccall "aPrimSet"    setObj#    :: H.JSString -> H.JSAny -> H.JSAny -> IO ()

-- | Returns the JavaScript global object.
global :: IO JsObject
global = global# >>= (return . JsObject)

-- | Creates a new JavaScript object. Equivalent to the literal value @{}@.
new :: IO JsObject
new = new# >>= (return . JsObject)

type JsName = String

-- | Class of types that can be properties of a JsObject.
class JsProp a where
    get     :: JsName -> JsObject -> IO a
    set     :: JsName -> JsObject -> a -> IO ()
    update  :: JsName -> JsObject -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

instance JsProp String where
    get name obj = getString# (H.toJSStr name) (getJsObject obj) >>= (return . H.fromJSStr)
    set name obj value = setString# (H.toJSStr name) (getJsObject obj) (H.toJSStr value)

instance JsProp Int where
    get name obj = getInt# (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setInt# (H.toJSStr name) (getJsObject obj) value

instance JsProp JsObject where
    get name obj = getObj# (H.toJSStr name) (getJsObject obj) >>= (return . JsObject)
    set name obj value = setObj# (H.toJSStr name) (getJsObject obj) (getJsObject value)

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
windowAlert :: String -> IO ()
windowAlert str  = alert# (H.toJSStr $ str)

-- | Like @window.console.log@ in JavaScript, i.e. posts a line to the web console.
windowConsoleLog :: String -> IO ()
windowConsoleLog str = consoleLog# (H.toJSStr $ str)

-- | Like @window.document.write@ in JavaScript, i.e. appends the given content at the end of the `body` element.
windowDocumentWrite :: String -> IO ()
windowDocumentWrite str = documentWrite# (H.toJSStr $ "<code>" ++ str ++ "</code><br/>")
