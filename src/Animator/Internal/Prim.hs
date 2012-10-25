
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances, ForeignFunctionInterface, OverloadedStrings #-}

-------------------------------------------------------------------------------------
-- | 
-- This module provides a basic interface to the JavaScript host environment.
-------------------------------------------------------------------------------------

-- TODO Remove TypeSynonymInstances, FlexibleInstances?
--      Required for String instance of JsProp, can we rethink this?

module Animator.Internal.Prim (

        -- ** Strings
        JsString,
        toJsString,
        fromJsString,

        -- ** Functions
        JsFun,
        call,
        JsArg(..),

        -- ** Objects
        JsObject,
        -- JsArray,
        new,
        create,
        typeOf,
        global,
        JsName,
        JsProp(..),

        -- ** JSON
        JSON,

        -- ** Utility
        windowAlert,
        windowConsoleLog,
        windowDocumentWrite,
  ) where

import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Semigroup
import qualified Haste.Prim as H
import qualified Haste.JSON as HJ

foreign import ccall "aPrimObj"    new#       :: IO H.JSAny
foreign import ccall "aPrimGlobal" global#    :: IO H.JSAny

foreign import ccall "aPrimGet"    getInt#    :: Int -> H.JSString -> H.JSAny -> IO Int
foreign import ccall "aPrimGet"    getWord#   :: Int -> H.JSString -> H.JSAny -> IO Word
foreign import ccall "aPrimGet"    getInt32#  :: Int -> H.JSString -> H.JSAny -> IO Int32
foreign import ccall "aPrimGet"    getWord32# :: Int -> H.JSString -> H.JSAny -> IO Word32
foreign import ccall "aPrimGet"    getFloat#  :: Int -> H.JSString -> H.JSAny -> IO Float
foreign import ccall "aPrimGet"    getDouble# :: Int -> H.JSString -> H.JSAny -> IO Double
foreign import ccall "aPrimGet"    getString# :: Int -> H.JSString -> H.JSAny -> IO H.JSString
foreign import ccall "aPrimGet"    getObj#    :: Int -> H.JSString -> H.JSAny -> IO H.JSAny

foreign import ccall "aPrimSet"    setInt#    :: Int -> H.JSString -> H.JSAny -> Int -> IO ()
foreign import ccall "aPrimGet"    setWord#   :: Int -> H.JSString -> H.JSAny -> Word -> IO ()
foreign import ccall "aPrimGet"    setInt32#  :: Int -> H.JSString -> H.JSAny -> Int32 -> IO ()
foreign import ccall "aPrimGet"    setWord32# :: Int -> H.JSString -> H.JSAny -> Word32 -> IO ()
foreign import ccall "aPrimGet"    setFloat#  :: Int -> H.JSString -> H.JSAny -> Float -> IO ()
foreign import ccall "aPrimGet"    setDouble# :: Int -> H.JSString -> H.JSAny -> Double -> IO ()
foreign import ccall "aPrimSet"    setString# :: Int -> H.JSString -> H.JSAny -> H.JSString -> IO ()
foreign import ccall "aPrimSet"    setObj#    :: Int -> H.JSString -> H.JSAny -> H.JSAny -> IO ()

foreign import ccall "aPrimAdd"    concatStr# :: H.JSString -> H.JSString -> H.JSString
foreign import ccall "aPrimTypeOf" typeOf#    :: H.JSAny -> H.JSString

foreign import ccall "aPrimWrite"  documentWrite#    :: H.JSString -> IO ()
foreign import ccall "aPrimLog"    consoleLog#       :: H.JSString -> IO ()
foreign import ccall "aPrimAlert"  alert#            :: H.JSString -> IO ()


-------------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------------

-- | 
-- An unboxed JavaScript string.
newtype JsString = JsString { getJsString :: H.JSString }
    deriving (Eq, Ord, Show, IsString)

instance Semigroup JsString where
    (JsString x) <> (JsString y) = JsString $ concatStr# x y
instance Monoid JsString where
    mappend = (<>)
    mempty = ""
    
-- Applicative
-- Monad
-- Functor
-- Semigroup
-- Foldable

-- | 
-- Convert a Haskell string to a JavaScript string.
toJsString :: String -> JsString  
toJsString = JsString . H.toJSStr 

-- | 
-- Convert a JavaScript string to a Haskell string.
fromJsString :: JsString -> String
fromJsString = H.fromJSStr . getJsString


-------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------

-- | 
-- An unboxed JavaScript function.
newtype JsFun a b = JsFun { getJsFun :: H.JSAny }

call :: (JsArg a, JsArg b) => JsFun a b -> a -> IO b
call = error "Not implemented"

-- |
-- Class of values that can be passed to a 'JsFun'.
class JsArg a where

-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

-- | 
-- An unboxed JavaScript object.
newtype JsObject = JsObject { getJsObject :: H.JSAny }

-- | 
-- Creates a new JavaScript object, or equivalently
--
-- > {}
new :: IO JsObject
new = new# >>= (return . JsObject)

-- | 
-- Creates a new JavaScript object using the given object as prototype, or equivalently
--
-- > Object.create(x)
create :: JsObject -> IO JsObject
create x = error "Not implemented"
                                      
-- | 
-- Returns the JavaScript global object.
global :: IO JsObject
global = global# >>= (return . JsObject)

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > typeof x
--
-- By definition, this function returns @\"object\"@, @\"function\"@
-- @\"xml\"@ or @\"undefined\"@.
typeOf :: JsObject -> String
typeOf = H.fromJSStr . typeOf# . getJsObject

-- |
-- A JavaScript property name.
type JsName = String

-- | 
-- Class of types that can be properties of a 'JsObject'.
-- 
-- Retrieving a value of the wrong type (for example, reading an 'Int' from a field 
-- containing a string) results in a runtime error.
class JsProp a where
    -- | @get n o@ fetches the value named @n@ from object @o@, or equivalently
    --
    -- > o.n    
    get :: JsName -> JsObject -> IO a

    -- | @set n o x@ assigns the property @n@ to @x@ in object @o@, or equivalently
    --
    -- > o.n = x
    set :: JsName -> JsObject -> a -> IO ()

    -- | @update n o f@ updates the value named @n@ in object @o@ by applying the function f, 
    --   or equivalently
    --
    -- > x.n = f(x.n)
    update :: JsName -> JsObject -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

kNumberType = 0
kStringType = 1
kObjectType = 2

instance JsProp Int where
    get name obj = getInt# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setInt# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp Word where
    get name obj = getWord# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setWord# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp Int32 where
    get name obj = getInt32# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setInt32# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp Word32 where
    get name obj = getWord32# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setWord32# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp Float where
    get name obj = getFloat# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setFloat# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp Double where
    get name obj = getDouble# kNumberType (H.toJSStr name) (getJsObject obj) >>= return
    set name obj value = setDouble# kNumberType (H.toJSStr name) (getJsObject obj) value

instance JsProp String where
    get name obj = getString# kStringType (H.toJSStr name) (getJsObject obj) >>= (return . H.fromJSStr)
    set name obj value = setString# kStringType (H.toJSStr name) (getJsObject obj) (H.toJSStr value)

instance JsProp JsObject where
    get name obj = getObj# kObjectType (H.toJSStr name) (getJsObject obj) >>= (return . JsObject)
    set name obj value = setObj# kObjectType (H.toJSStr name) (getJsObject obj) (getJsObject value)


-------------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------------

-- TODO
newtype JSON = JSON { getJSON :: HJ.JSON }


-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

-- | 
-- Displays a modal window with the given text.
windowAlert :: String -> IO ()
windowAlert str  = alert# (H.toJSStr $ str)

-- | 
-- Posts a line to the web console.
windowConsoleLog :: String -> IO ()
windowConsoleLog str = consoleLog# (H.toJSStr $ str)

-- | 
-- Appends the given content at the end of the `body` element.
windowDocumentWrite :: String -> IO ()
windowDocumentWrite str = documentWrite# (H.toJSStr $ "<code>" ++ str ++ "</code><br/>")


