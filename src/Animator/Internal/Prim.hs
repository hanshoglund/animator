
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances, ForeignFunctionInterface, OverloadedStrings, CPP #-}

-------------------------------------------------------------------------------------
-- |
-- This module provides a basic interface to the JavaScript host environment.
-------------------------------------------------------------------------------------

-- TODO Remove TypeSynonymInstances, FlexibleInstances?
--      Required for String instance of JsProp, can we rethink this?

module Animator.Internal.Prim (
        JsRef(..),
        JsValue,
        typeOf,
        JsName,
        JsProp(..),

        -- ** Strings
        JsString,
        toJsString,
        fromJsString,
        charAt,
        indexOf,
        lastIndexOf,
        -- match
        -- replace
        -- search
        -- split
        sliceString,
        toLower,
        toUpper,
        
        -- ** Functions
        JsFunction,
        arity,
        bind,
        apply,
        new,

        -- ** Arrays
        JsArray,
        push,
        pop,
        shift,
        unshift,
        reverse,
        sort,
        -- splice,
        join,
        sliceArray,
        
        -- ** Objects
        JsObject,
        -- undef,
        object,
        create,
        null,
        global,
        isInstanceOf,
        isPrototypeOf,
        constructor,
        hasOwnProperty,
        propertyIsEnumerable,

        -- ** JSON
        JSON,
        parse,
        stringify,

        -- ** Utility
        windowAlert,
        windowConsoleLog,
        windowDocumentWrite,
        
        JsString,
        JsObject,
        getJsString,
        getJsObject,
        logPrim,
        eval
  ) where

import Prelude hiding (reverse, null)

import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Semigroup
import Unsafe.Coerce

#ifdef __HASTE__
import qualified Haste.Prim as H
import qualified Haste.JSON as HJ
#else
import Foreign.Ptr
#endif __HASTE__


#ifdef __HASTE__
type Any#     = H.JSAny              -- Opaque reference
type Fun#     = (Any# -> IO Any#)    -- Opaque unary function
type FunPtr#  = H.Ptr Fun#           -- Unboxed JS string
type String#  = H.JSString
type JSON#    = HJ.JSON
toJsString#   = H.toJSStr
fromJsString# = H.fromJSStr
#else
type Any#     = Ptr Int
type Fun#     = (Any# -> IO Any#)
type FunPtr#  = Ptr Fun#
type String#  = Int
type JSON#    = Int
toJsString#   = undefined
fromJsString# = undefined
#endif __HASTE__

numberType#   = 0
stringType#   = 1
objectType#   = 2
functionType# = 3

foreign import ccall "aPrimObj"       object#           :: IO Any#
foreign import ccall "aPrimGlobal"    global#           :: IO Any#

foreign import ccall "aPrimGet"       getInt#           :: Int -> String# -> Any# -> IO Int
foreign import ccall "aPrimGet"       getWord#          :: Int -> String# -> Any# -> IO Word
foreign import ccall "aPrimGet"       getInt32#         :: Int -> String# -> Any# -> IO Int32
foreign import ccall "aPrimGet"       getWord32#        :: Int -> String# -> Any# -> IO Word32
foreign import ccall "aPrimGet"       getFloat#         :: Int -> String# -> Any# -> IO Float
foreign import ccall "aPrimGet"       getDouble#        :: Int -> String# -> Any# -> IO Double
foreign import ccall "aPrimGet"       getString#        :: Int -> String# -> Any# -> IO String#
foreign import ccall "aPrimGet"       getAny#           :: Int -> String# -> Any# -> IO Any#

foreign import ccall "aPrimSet"       setInt#           :: Int -> String# -> Any# -> Int     -> IO ()
foreign import ccall "aPrimSet"       setWord#          :: Int -> String# -> Any# -> Word    -> IO ()
foreign import ccall "aPrimSet"       setInt32#         :: Int -> String# -> Any# -> Int32   -> IO ()
foreign import ccall "aPrimSet"       setWord32#        :: Int -> String# -> Any# -> Word32  -> IO ()
foreign import ccall "aPrimSet"       setFloat#         :: Int -> String# -> Any# -> Float   -> IO ()
foreign import ccall "aPrimSet"       setDouble#        :: Int -> String# -> Any# -> Double  -> IO ()
foreign import ccall "aPrimSet"       setString#        :: Int -> String# -> Any# -> String# -> IO ()
foreign import ccall "aPrimSet"       setAny#           :: Int -> String# -> Any# -> Any#    -> IO ()

foreign import ccall "aPrimArrConcat" concatArray#      :: Any# -> Any# -> Any#
foreign import ccall "aPrimAdd"       concatString#     :: String# -> String# -> String#
foreign import ccall "aPrimTypeOf"    typeOf#           :: Any# -> String#

foreign import ccall "aPrimWrite"     documentWrite#    :: String# -> IO ()
foreign import ccall "aPrimLog"       consoleLog#       :: String# -> IO ()
foreign import ccall "aPrimAlert"     alert#            :: String# -> IO ()

foreign import ccall "aPrimEval"      eval#             :: String# -> IO Any#

foreign import ccall "aPrimLog"       logAny#           :: Any# -> IO ()

logPrim :: a -> IO ()
logPrim = logAny# . unsafeCoerce . H.toPtr
{-# NOINLINE logPrim #-}

eval :: JsString -> IO a
eval = unsafeCoerce . eval# . getJsString


class JsRef a where
    toJsObject :: a -> JsObject
    toJsObject = error "Not implemented"
instance JsRef JsFunction where
instance JsRef JsArray where
instance JsRef JSON where
instance JsRef JsObject where
    toJsObject = id

-- |
-- Class of types that can be passed to a 'JsFunction'.
class JsValue a where
    bind# :: JsValue a => JsFunction -> JsObject -> [a] -> JsFunction
    apply# :: JsValue a => JsFunction -> JsObject -> [a] -> a
    new# :: JsValue a => JsFunction -> [a] -> IO JsObject

-- instance JsValue Int where
-- instance JsValue Int32 where
-- instance JsValue Word where
-- instance JsValue Word32 where
-- instance JsValue Float where
-- instance JsValue Double where
-- instance JsValue JsString where
-- instance JsValue JsObject where
-- instance JsValue JsArray where
-- instance JsValue JsFunction where
    

-------------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------------

-- |
-- An unboxed JavaScript string.
newtype JsString = JsString { getJsString :: String# }
    deriving (Eq, Ord, Show)

instance IsString JsString where
    fromString = toJsString
instance Semigroup JsString where
    (JsString x) <> (JsString y) = JsString $ x `concatString#` y
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
toJsString = JsString . toJsString#

-- |
-- Convert a JavaScript string to a Haskell string.
fromJsString :: JsString -> String
fromJsString = fromJsString# . getJsString

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.charAt.call(s, i)
charAt :: Int -> JsString -> JsString
charAt = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.indexOf.call(s, c)
indexOf :: JsString -> JsString -> Int
indexOf = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.lastIndexOf.call(s, c)
lastIndexOf :: JsString -> JsString -> Int
lastIndexOf = error "Not implemented"

-- match
-- replace
-- search
-- split

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.slice.call(s, a, b)
sliceString :: Int -> Int -> JsString -> JsString
sliceString = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.toLowerCase.call(s)
toLower :: JsString -> JsString
toLower = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.toUpperCase.call(s)
toUpper :: JsString -> JsString
toUpper = error "Not implemented"


-------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------

-- |
-- An unboxed JavaScript function.
newtype JsFunction = JsFunction { getJsFunction :: Any# }

arity :: JsFunction -> Int
arity = error "Not implemented"

-- |
-- Partially apply the given function, or equivalently
--
-- > Function.prototype.bind.call(f, x, ... as)
bind :: JsValue a => JsFunction -> a -> [a] -> JsFunction
bind = error "Not implemented"

-- |
-- Apply the given function, or equivalently
--
-- > Function.prototype.apply.call(f, x, as)
apply :: JsValue a => JsFunction -> a -> [a] -> a
apply = error "Not implemented"

-- |
-- Invokes the given function as a constructor, or equivalently
--
-- > new F(args)
new :: JsValue a => JsFunction -> [a] -> IO JsObject
new = error "Not implemented"


-------------------------------------------------------------------------------------
-- Arrays
-------------------------------------------------------------------------------------

-- |
-- An unboxed JavaScript array.
newtype JsArray = JsArray { getJsArray :: Any# }

instance Semigroup JsArray where
    (JsArray x) <> (JsArray y) = JsArray $ concatArray# x y
instance Monoid JsArray where
    mappend = (<>)
    mempty = error "Not implemented"

-- Applicative
-- Monad
-- Functor
-- Semigroup
-- Foldable

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.pop.call(x)
pop :: JsValue a => JsArray -> IO a
pop = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.push.call(x, v)
push :: JsValue a => a -> JsArray -> IO JsArray
push = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.shift.call(x)
shift :: JsValue a => JsArray -> IO a
shift = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.unshift.call(x, v)
unshift :: JsValue a => a -> JsArray -> IO JsArray
unshift = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.reverse.call(x)
reverse :: JsArray -> IO JsArray
reverse = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.sort.call(x)
sort :: JsArray -> IO JsArray
sort = error "Not implemented"

-- splice

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.join.call(x, s)
join :: JsString -> JsArray -> JsString
join = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.slice.call(x, a, b)
sliceArray :: Int -> Int -> JsString -> JsString
sliceArray = error "Not implemented"


-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

-- |
-- An unboxed JavaScript object.
newtype JsObject = JsObject { getJsObject :: Any# }

-- |
-- Creates a new JavaScript object, or equivalently
--
-- > {}
object :: IO JsObject
object = object# >>= (return . JsObject)

-- |
-- Creates a new JavaScript object using the given object as prototype, or equivalently
--
-- > Object.create(x)
create :: JsObject -> IO JsObject
create x = error "Not implemented"

-- |
-- Returns the JavaScript null object, or equivalently
--
-- > null
null :: JsObject
null = error "Not implemented" 

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > window
global :: IO JsObject
global = global# >>= (return . JsObject)

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > typeof x
typeOf :: JsValue a => a -> String
typeOf = fromJsString# . typeOf# . undefined

-- |
-- Returns true if the specified object is of the specified object type, or equivalently
--
-- > x instanceof y
isInstanceOf :: JsObject -> JsObject -> Bool
isInstanceOf = error "Not implemented"

-- |
-- 
--
-- > x.isPrototypeOf(y)
isPrototypeOf :: JsObject -> JsObject -> Bool
isPrototypeOf = error "Not implemented"

-- |
-- 
--
-- > x.constructor(y)
constructor :: JsObject -> JsFunction
constructor = error "Not implemented"

-- |
-- 
--
-- > x.hasOwnProperty(y)
hasOwnProperty :: JsName -> JsObject -> IO Bool
hasOwnProperty = error "Not implemented"

-- |
-- 
--
-- > x.propertyIsEnumerable(y)
propertyIsEnumerable :: JsName -> JsObject -> IO Bool
propertyIsEnumerable = error "Not implemented"



-- |
-- Deletes the given property from an object, or equivalently
--
-- > delete o.n
delete :: JsName -> JsObject -> IO ()
delete = error "Not implemented"

toString :: JsObject -> JsString
toString = error "Not implemented"

toLocaleString :: JsObject -> JsString
toLocaleString = error "Not implemented"

valueOf :: JsValue a => JsObject -> a
valueOf = error "Not implemented"



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


get# i c n x = i (toJsString# n) (getJsObject x) >>= (return . c)
set# o c n x v = o (toJsString# n) (getJsObject x) (c v)

instance JsProp Int where
    get = get# (getInt# numberType#) id
    set = set# (setInt# numberType#) id

instance JsProp Word where
    get = get# (getWord# numberType#) id
    set = set# (setWord# numberType#) id

instance JsProp Int32 where
    get = get# (getInt32# numberType#) id
    set = set# (setInt32# numberType#) id

instance JsProp Word32 where
    get = get# (getWord32# numberType#) id
    set = set# (setWord32# numberType#) id

instance JsProp Float where
    get = get# (getFloat# numberType#) id
    set = set# (setFloat# numberType#) id

instance JsProp Double where
    get = get# (getDouble# numberType#) id
    set = set# (setDouble# numberType#) id

instance JsProp String where
    get = get# (getString# stringType#) fromJsString#
    set = set# (setString# stringType#) toJsString#

instance JsProp JsObject where
    get = get# (getAny# objectType#) JsObject
    set = set# (setAny# objectType#) getJsObject


-------------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------------

-- TODO
newtype JSON = JSON { getJSON :: JSON# }

parse :: JsString -> JSON
parse = error "Not implemented"

stringify :: JSON -> JsString
stringify = error "Not implemented"


-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

-- |
-- Displays a modal window with the given text.
windowAlert :: String -> IO ()
windowAlert str  = alert# (toJsString# $ str)

-- |
-- Posts a line to the web console.
windowConsoleLog :: String -> IO ()
windowConsoleLog str = consoleLog# (toJsString# $ str)

-- |
-- Appends the given content at the end of the `body` element.
windowDocumentWrite :: String -> IO ()
windowDocumentWrite str = documentWrite# (toJsString# $ "<code>" ++ str ++ "</code><br/>")


