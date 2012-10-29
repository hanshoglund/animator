
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings, 
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

-- |
-- The 'Animator.Primitive' modules provide a dynamic interface to the JavaScript host environment.
-- It provides access to JavaScript types including strings, arrays, objects and functions.
--
-- Objects can be inspected, created and updated at runtime. JavaScript functions can be called,
-- partially applied or passed to other JavaScript functions. Haskell functions can be converted
-- to JavaScript functions for use as callbacks in the host environment.
--

-------------------------------------------------------------------------------------

-- TODO Remove TypeSynonymInstances, FlexibleInstances?
--      Required for String instance of JsProp, can we rethink this?

module Animator.Internal.Prim (

        -- ** Classes
        -- *** All types
        JsVal(..),

        -- *** Reference types
        JsRef(..),
        JsName,

        -- *** Property assignment
        JsProp(..),
        (%%),
        lookup,

        -- ** Objects
        JsObject,

        -- *** Creation and access
        object,
        create,
        null,
        global,

        -- *** Prototype hierarchy
        isInstanceOf,
        isPrototypeOf,
        constructor,

        -- *** Properties
        delete,
        hasProperty,
        hasOwnProperty,
        propertyIsEnumerable,

        -- *** Conversion
        toString,
        toLocaleString,
        valueOf,



        -- ** Arrays
        JsArray,

        -- *** Creation and access
        array,
        length,
        push,
        pop,
        shift,
        unshift,

        -- *** Manipulation and conversion
        reverse,
        sort,
        -- splice,
        join,
        sliceArray,



        -- ** Strings
        JsString,

        -- *** Creation and access
        toJsString,
        fromJsString,
        charAt,

        -- *** Searching
        indexOf,
        lastIndexOf,
        -- match
        -- replace
        -- search
        -- split

        -- *** Manipulation and conversion
        sliceString,
        toLower,
        toUpper,

        -- ** Functions
        JsFunction,

        arity,
        call,
        call1,
        call2,
        invoke,
        invoke1,
        invoke2,

        -- *** Partial application
        bind,
        bind2,

        -- *** Lifting Haskell functions
        lift,
        lift1,
        lift2,
        liftIO,
        liftIO1,
        liftIO2,

        -- *** With explicit this argument
        callWith,
        callWith1,
        callWith2,

        bindWith,
        bindWith1,
        bindWith2,

        -- *** Infix versions
        (%),
        (%.),
        (%..),
        -- apply,
        -- new,

        -- ** JSON
        JSON,
        parse,
        stringify,

        -- ** Utility
        -- *** Eval
        eval,

        -- *** Print
        alert,
        printLog,
        printDoc,

        -- *** Debug
        printRepr,
        debug
  ) where

import Prelude hiding (reverse, null, length, lookup)

import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Semigroup  

import Unsafe.Coerce
import System.IO.Unsafe

#ifdef __HASTE__
import qualified Haste.Prim as H
import qualified Haste.JSON as HJ
#else
import Foreign.Ptr
#endif __HASTE__



#ifdef __HASTE__
type Any#     = H.JSAny              -- Opaque reference
type String#  = H.JSString
type JSON#    = HJ.JSON
toJsString#   = H.toJSStr
fromJsString# = H.fromJSStr
toPtr#        = H.toPtr
#else
type Any#     = Ptr Int
type String#  = Int
type JSON#    = Int
toJsString#   = undefined
fromJsString# = undefined
toPtr#        = undefined
#endif __HASTE__

numberType#   = 0
stringType#   = 1
objectType#   = 2
functionType# = 3




foreign import ccall "aPrimTypeOf"    typeOf#           :: Any# -> String#

-- |
-- Class of JavaScript types.
class JsVal a where
    -- | Returns a string describing a type of the given value, or equivalently
    --
    -- > typeof x
    --
    -- By definition, this function returns one of the following strings:
    --
    -- > "undefined", "boolean", "number", "string", "object", "function"
    -- 
    -- /ECMA-262 11.4.3/
    typeOf :: JsVal a => a -> String
    typeOf = fromJsString# . typeOf# . unsafeCoerce

instance JsVal () where
    typeOf () = "object"
-- instance JsVal Bool where -- TODO
instance JsVal Int where
    typeOf _ = "number"
instance JsVal Int32 where
    typeOf _ = "number"
instance JsVal Word where
    typeOf _ = "number"
instance JsVal Word32 where
    typeOf _ = "number"
instance JsVal Float where
    typeOf _ = "number"
instance JsVal Double where
    typeOf _ = "number"
instance JsVal JsString where
    typeOf _ = "string"
instance JsVal JsObject where
    -- typeOf = default
instance JsVal JsArray where
    typeOf _ = "object"
instance JsVal JsFunction where
    typeOf _ = "function"
-- instance JsVal (Ptr a) where -- TODO
--     typeOf _ = "object"


-- |
-- Class of JavaScript reference types.
class JsVal a => JsRef a where
    toObject :: a -> JsObject
instance JsRef JsFunction where
    toObject = unsafeCoerce
instance JsRef JsArray where
    toObject = unsafeCoerce
instance JsRef JsObject where
    toObject = id


-- |
-- A JavaScript property name.
type JsName = String

-- |
-- Class of types that can be properties of a 'JsObject'.
--
-- Retrieving a value of the wrong type (for example, reading an 'Int' from a field
-- containing a string) results in a runtime error.
class JsVal a => JsProp a where
    -- | Fetches the value of property @n@ in object @o@, or equivalently
    --
    -- > o.n
    get :: JsObject -> JsName -> IO a

    -- | Assigns the property @n@ to @x@ in object @o@, or equivalently
    --
    -- > o.n = x
    set :: JsObject -> JsName -> a -> IO ()

    -- | Updates the value named @n@ in object @o@ by applying the function f,
    --   or equivalently
    --
    -- > x.n = f(x.n)
    update :: JsObject -> JsName -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

lookup :: JsProp a => JsObject -> [JsName] -> IO a
lookup o [] = error "lookup: Empty list"
lookup o (x:xs) = do
    o' <- lookup' o xs
    get o' x

lookup' :: JsObject -> [JsName] -> IO JsObject
lookup' o [] = return $ o
lookup' o (x:xs) = do
    o' <- lookup' o xs
    get o' x 

_lookupGlobal xs = unsafePerformIO $ global >>= \g -> lookup g xs

_Object             = _lookupGlobal ["Object"]
_Object_prototype   = _lookupGlobal ["Object", "prototype"]
_Array              = _lookupGlobal ["Array"]
_Array_prototype    = _lookupGlobal ["Array", "prototype"]
_Function           = _lookupGlobal ["Function"]
_Function_prototype = _lookupGlobal ["Function", "prototype"]
_String             = _lookupGlobal ["String"]
_String_prototype   = _lookupGlobal ["String", "prototype"]
_JSON               = _lookupGlobal ["JSON"]


-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

foreign import ccall "aPrimNull"       null#             :: Any#
foreign import ccall "aPrimObj"        object#           :: IO Any#
foreign import ccall "aPrimGlobal"     global#           :: IO Any#
foreign import ccall "aPrimInstanceOf" instanceOf#       :: Any# -> Any# -> Int

-- |
-- A JavaScript object.
--
-- This type is disjoint from ordinary Haskell data types, which have a compiler-specific
-- internal representation. All JavaScript reference types can be converted to this type
-- using the 'JsRef' instancce.
--
--  /ECMA-262 8.6, 15.2/
--
newtype JsObject = JsObject { getJsObject :: Any# }

-- |
-- Returns the JavaScript null object, or equivalently
--
-- > null
null :: JsObject
null = JsObject $ null#

-- |
-- Creates a new JavaScript object, or equivalently
--
-- > {}
object :: IO JsObject
object = object# >>= (return . JsObject)

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > window
global :: IO JsObject
global = global# >>= (return . JsObject)

-- |
-- Creates a new JavaScript object using the given object as prototype, or equivalently
--
-- > Object.create(x)
create :: JsObject -> IO JsObject
create = _Object %. "create"

-- |
-- Returns true if the specified object is of the specified object type, or equivalently
--
-- > x instanceof y
isInstanceOf :: JsObject -> JsObject -> Int
x `isInstanceOf` y = p x `instanceOf#` p y 
    where p = getJsObject

-- |
-- Returns
--
-- > x.isPrototypeOf(y)
isPrototypeOf :: JsObject -> JsObject -> Int
x `isPrototypeOf` y = q $ (x %. "isPrototypeOf") y
    where q = unsafePerformIO
    
-- |
-- Returns
--
-- > x.constructor
constructor :: JsObject -> JsFunction
constructor x = q $ x %% "constructor"
    where q = unsafePerformIO

foreign import ccall "aPrimHas"    has#    :: Int -> Any# -> String# -> IO Int
foreign import ccall "aPrimDelete" delete# :: Int -> Any# -> String# -> IO Int

-- |
-- Deletes the property @n@ form object @o@, or equivalently
--
-- > delete o.n
delete :: JsObject -> JsName -> IO ()
delete x n = delete# 0 (getJsObject x) (toJsString# n) >> return ()

-- |
-- Returns
--
-- > o.n !== undefined
hasProperty :: JsObject -> JsName -> IO Int
hasProperty x n = has# 0 (getJsObject x) (toJsString# n)

-- |
-- Returns
--
-- > x.hasOwnProperty(y)
hasOwnProperty :: JsObject -> JsName -> IO Int
hasOwnProperty x n = (x %. "hasOwnProperty") (toJsString n)

-- |
-- Returns
--
-- > x.propertyIsEnumerable(y)
propertyIsEnumerable :: JsObject -> JsName -> IO Int
propertyIsEnumerable x n = (x %. "propertyIsEnumerable") (toJsString n)


-- |
-- Returns
--
-- > x.toString(y)
toString :: JsObject -> IO JsString
toString x = x % "toString"

-- |
-- Returns
--
-- > x.toLocaleString(y)
toLocaleString :: JsObject -> IO JsString
toLocaleString x = x % "toLocaleString"

-- |
-- Returns
--
-- > x.valueOf(y)
valueOf :: JsVal a => JsObject -> IO a
valueOf x = x % "valueOf"


foreign import ccall "aPrimGet"       getInt#           :: Int -> Any# -> String# -> IO Int
foreign import ccall "aPrimGet"       getWord#          :: Int -> Any# -> String# -> IO Word
foreign import ccall "aPrimGet"       getInt32#         :: Int -> Any# -> String# -> IO Int32
foreign import ccall "aPrimGet"       getWord32#        :: Int -> Any# -> String# -> IO Word32
foreign import ccall "aPrimGet"       getFloat#         :: Int -> Any# -> String# -> IO Float
foreign import ccall "aPrimGet"       getDouble#        :: Int -> Any# -> String# -> IO Double
foreign import ccall "aPrimGet"       getString#        :: Int -> Any# -> String# -> IO String#
foreign import ccall "aPrimGet"       getAny#           :: Int -> Any# -> String# -> IO Any#

foreign import ccall "aPrimSet"       setInt#           :: Int -> Any# -> String# -> Int     -> IO ()
foreign import ccall "aPrimSet"       setWord#          :: Int -> Any# -> String# -> Word    -> IO ()
foreign import ccall "aPrimSet"       setInt32#         :: Int -> Any# -> String# -> Int32   -> IO ()
foreign import ccall "aPrimSet"       setWord32#        :: Int -> Any# -> String# -> Word32  -> IO ()
foreign import ccall "aPrimSet"       setFloat#         :: Int -> Any# -> String# -> Float   -> IO ()
foreign import ccall "aPrimSet"       setDouble#        :: Int -> Any# -> String# -> Double  -> IO ()
foreign import ccall "aPrimSet"       setString#        :: Int -> Any# -> String# -> String# -> IO ()
foreign import ccall "aPrimSet"       setAny#           :: Int -> Any# -> String# -> Any#    -> IO ()

get# i c x n   = i (getJsObject x) (toJsString# n)  >>= (return . c)
set# o c x n v = o (getJsObject x) (toJsString# n) (c v)

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

instance JsProp JsString where
    get = get# (getString# stringType#) JsString
    set = set# (setString# stringType#) getJsString

instance JsProp JsObject where
    get = get# (getAny# objectType#) JsObject
    set = set# (setAny# objectType#) getJsObject

instance JsProp JsFunction where
    get = get# (getAny# functionType#) JsFunction
    set = set# (setAny# functionType#) getJsFunction

-- instance JsProp String where
--     get = get# (getString# stringType#) fromJsString#
--     set = set# (setString# stringType#) toJsString#



-------------------------------------------------------------------------------------
-- Arrays
-------------------------------------------------------------------------------------

foreign import ccall "aPrimArr"       array#            :: IO Any#
foreign import ccall "aPrimArrConcat" concatArray#      :: Any# -> Any# -> Any#

-- |
-- A JavaScript array.
--
--  /ECMA-262 15.4/
--
newtype JsArray = JsArray { getJsArray :: Any# }

-- instance Semigroup JsArray where
--     (JsArray x) <> (JsArray y) = JsArray $ concatArray# x y
-- instance Monoid JsArray where
--     mappend = (<>)
--     mempty  = array

-- Applicative
-- Monad
-- Functor
-- Semigroup
-- Foldable

-- |
-- Returns
--
-- > []
array :: IO JsArray
array = array# >>= (return . JsArray)

-- |
-- Returns the length of the given array, or equivalently
--
-- > xs.length
length :: JsArray -> IO Int
length xs = (toObject xs) %% "length"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.pop()
pop :: JsVal a => JsArray -> IO a
pop = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.push(x)
push :: JsVal a => a -> JsArray -> IO JsArray
push = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.shift()
shift :: JsVal a => JsArray -> IO a
shift = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.shift(x)
unshift :: JsVal a => a -> JsArray -> IO JsArray
unshift = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.reverse()
reverse :: JsArray -> IO JsArray
reverse = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.sort()
sort :: JsArray -> IO JsArray
sort = error "Not implemented"

-- splice

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.join.call(x, s)
join :: JsArray -> JsArray -> IO JsString
join = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.slice.call(x, a, b)
sliceArray :: Int -> Int -> JsArray -> IO JsArray
sliceArray = error "Not implemented"



-------------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------------

-- |
-- A JavaScript string.
--
-- Defined as an immutable sequence of Unicode characters. Allthough this type is
-- normally used for text, it can be used to store any unsigned 16-bit value using
-- 'charAt' and 'fromCharCode'. Operations on 'JsString' are normally magnitudes faster
-- than the equivalent 'String' operation; on the other hand the full range of
-- 'Data.Char' and 'Data.List' functions are not available.
--
-- There is no 'Char' type in JavaScript, so functions dealing with single characters
-- return singleton strings.
--
-- /ECMA-262 8.4, 15.5/
--
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

foreign import ccall "aPrimAdd" concatString# :: String# -> String# -> String#

-- |
-- A JavaScript function, i.e. a callable object.
--
-- This type is disjoint from ordinary Haskell functions, which have a compiler-specific
-- internal representation. To convert between the two, use 'call', 'lift' or 'liftIO'.
--
-- /ECMA-262 9.11, 15.3/
--
newtype JsFunction = JsFunction { getJsFunction :: Any# }

-- |
-- Returns the arity of the given function, or equivalently
--
-- > f.length
arity :: JsFunction -> Int
arity = error "Not implemented"

foreign import ccall "aPrimCall0" call#  :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall1" call1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall2" call2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall3" call3#  :: Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall4" call4#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall5" call5#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#

-- |
-- Apply the given function, or equivalently
--
-- > f()
call :: JsVal a => JsFunction -> IO a

-- |
-- Apply the given function, or equivalently
--
-- > f(a)
call1 :: (JsVal a, JsVal b) => JsFunction -> a -> IO b

-- |
-- Apply the given function, or equivalently
--
-- > f(a, b)
call2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> IO c

-- |
-- Apply the given function, or equivalently
--
-- > f(a, b, c)
call3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> a -> b -> c -> IO d

-- |
-- Apply the given function, or equivalently
--
-- > f(a, b, c, d)
call4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> a -> b -> c -> d -> IO e

call  f = callWith  f null
call1 f = callWith1 f null
call2 f = callWith2 f null
call3 f = callWith3 f null
call4 f = callWith4 f null

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg)
callWith :: JsVal a => JsFunction -> JsObject -> IO a

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a)
callWith1 :: (JsVal a, JsVal b) => JsFunction -> JsObject -> a -> IO b

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a, b)
callWith2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> JsObject -> a -> b -> IO c
callWith3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> JsObject -> a -> b -> c -> IO d
callWith4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> JsObject -> a -> b -> c -> d -> IO e
callWith5 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsFunction -> JsObject -> a -> b -> c -> d -> e -> IO f

callWith f t = do
    r <- call# (getJsFunction f) (p t)
    return $ q r
    where
        (p,q) = callPrePost

callWith1 f t a = do
    r <- call1# (getJsFunction f) (p t) (p a)
    return $ q r
    where
        (p,q) = callPrePost

callWith2 f t a b = do
    r <- call2# (getJsFunction f) (p t) (p a) (p b)
    return $ q r
    where
        (p,q) = callPrePost

callWith3 f t a b c = do
    r <- call3# (getJsFunction f) (p t) (p a) (p b) (p c)
    return $ q r
    where
        (p,q) = callPrePost

callWith4 f t a b c d = do
    r <- call4# (getJsFunction f) (p t) (p a) (p b) (p c) (p d)
    return $ q r
    where
        (p,q) = callPrePost

callWith5 f t a b c d e = do
    r <- call5# (getJsFunction f) (p t) (p a) (p b) (p c) (p d) (p e)
    return $ q r
    where
        (p,q) = callPrePost

foreign import ccall "aPrimBind0" bind#  :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimBind1" bind1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimBind2" bind2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimBind3" bind3#  :: Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimBind4" bind4#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimBind5" bind5#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a)
bind :: JsVal a => JsFunction -> a -> IO JsFunction

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a, b)
bind2 :: (JsVal a, JsVal b) => JsFunction -> a -> b -> IO JsFunction

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a, b, c)
bind3 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> c -> IO JsFunction

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a, b, c, d)
bind4 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> a -> b -> c -> d -> IO JsFunction

bind  f = bindWith1 f null
bind2 f = bindWith2 f null
bind3 f = bindWith3 f null
bind4 f = bindWith4 f null

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg)
bindWith :: JsFunction -> JsObject -> IO JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a)
bindWith1 :: JsVal a => JsFunction -> JsObject -> a -> IO JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a, b)
bindWith2 :: (JsVal a, JsVal b) => JsFunction -> JsObject -> a -> b -> IO JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a, b, c)
bindWith3 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> JsObject -> a -> b -> c -> IO JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a, b, c, d)
bindWith4 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> JsObject -> a -> b -> c -> d -> IO JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a, b, c, d, e)
bindWith5 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> JsObject -> a -> b -> c -> d -> e -> IO JsFunction

bindWith f t = do
    r <- bind# (getJsFunction f) (p t)
    return $ q r
    where
        (p,q) = bindPrePost

bindWith1 f t a = do
    r <- bind1# (getJsFunction f) (p t) (p a)
    return $ q r
    where
        (p,q) = bindPrePost

bindWith2 f t a b = do
    r <- bind2# (getJsFunction f) (p t) (p a) (p b)
    return $ q r
    where
        (p,q) = bindPrePost

bindWith3 f t a b c = do
    r <- bind3# (getJsFunction f) (p t) (p a) (p b) (p c)
    return $ q r
    where
        (p,q) = bindPrePost

bindWith4 f t a b c d = do
    r <- bind4# (getJsFunction f) (p t) (p a) (p b) (p c) (p d)
    return $ q r
    where
        (p,q) = bindPrePost

bindWith5 f t a b c d e = do
    r <- bind5# (getJsFunction f) (p t) (p a) (p b) (p c) (p d) (p e)
    return $ q r
    where
        (p,q) = bindPrePost

callPrePost = (unsafeCoerce, unsafeCoerce)
bindPrePost = (unsafeCoerce, unsafeCoerce)


infixl 1 %
infixl 1 %.
infixl 1 %..
infixl 1 %%

-- |
-- Infix version of 'invoke'.
(%)   = invoke

-- |
-- Infix version of 'invoke1'.
(%.)  = invoke1

-- |
-- Infix version of 'invoke2'.
(%..) = invoke2

-- |
-- Infix version of 'get'.
(%%) = get



-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n()
invoke :: JsVal a => JsObject -> JsName -> IO a

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a)
invoke1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> IO b

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b)
invoke2 :: (JsVal a, JsVal b, JsVal c) => JsObject -> JsName -> a -> b -> IO c

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b, c)
invoke3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsObject -> JsName -> a -> b -> c -> IO d

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b, c, d)
invoke4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsObject -> JsName -> a -> b -> c -> d -> IO e

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b, c, d, e)
invoke5 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsObject -> JsName -> a -> b -> c -> d -> e -> IO f

invoke o n = do
    f <- get o n
    callWith f o

invoke1 o n a = do
    f <- get o n
    callWith1 f o a

invoke2 o n a b = do
    f <- get o n
    callWith2 f o a b

invoke3 o n a b c = do
    f <- get o n
    callWith3 f o a b c

invoke4 o n a b c d = do
    f <- get o n
    callWith4 f o a b c d

invoke5 o n a b c d e = do
    f <- get o n
    callWith5 f o a b c d e

-- -- |
-- -- Partially apply the given function, or equivalently
-- --
-- -- > Function.prototype.bind.call(f, x, ... as)
-- bind :: JsVal a => JsFunction -> a -> [a] -> JsFunction
-- bind = error "Not implemented"
--
-- -- |
-- -- Apply the given function, or equivalently
-- --
-- -- > Function.prototype.apply.call(f, x, as)
-- apply :: JsVal a => JsFunction -> a -> [a] -> a
-- apply = error "Not implemented"
--
-- -- |
-- -- Invokes the given function as a constructor, or equivalently
-- --
-- -- > new F(args)
-- new :: JsVal a => JsFunction -> [a] -> IO JsObject
-- new = error "Not implemented"

foreign import ccall "aPrimLiftPure0" liftPure#   :: Any# -> Any#
foreign import ccall "aPrimLiftPure1" liftPure1#  :: Any# -> Any#
foreign import ccall "aPrimLiftPure2" liftPure2#  :: Any# -> Any#
foreign import ccall "aPrimLift0" lift#   :: Any# -> Any#
foreign import ccall "aPrimLift1" lift1#  :: Any# -> Any#
foreign import ccall "aPrimLift2" lift2#  :: Any# -> Any#

-- |
-- Lift the given Haskell function into a JavaScript function
lift :: JsVal a => a -> JsFunction

-- |
-- Lift the given Haskell function into a JavaScript function
lift1 :: (JsVal a, JsVal b) => (a -> b) -> JsFunction

-- |
-- Lift the given Haskell function into a JavaScript function
lift2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> c) -> JsFunction

-- |
-- Lift the given Haskell function into a JavaScript function
liftIO :: JsVal a => IO a -> JsFunction

-- |
-- Lift the given Haskell function into a JavaScript function
liftIO1 :: (JsVal a, JsVal b) => (a -> IO b) -> JsFunction

-- |
-- Lift the given Haskell function into a JavaScript function
liftIO2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> IO c) -> JsFunction


lift  = JsFunction . liftPure#  . unsafeCoerce . toPtr#
lift1 = JsFunction . liftPure1# . unsafeCoerce . toPtr#
lift2 = JsFunction . liftPure2# . unsafeCoerce . toPtr#
liftIO  = JsFunction . lift#  . unsafeCoerce . toPtr#
liftIO1 = JsFunction . lift1# . unsafeCoerce . toPtr#
liftIO2 = JsFunction . lift2# . unsafeCoerce . toPtr#


-------------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------------

--  /ECMA-262 15.12/
newtype JSON = JSON { getJSON :: JSON# }

parse :: JsString -> JSON
parse = error "Not implemented"

stringify :: JSON -> JsString
stringify = error "Not implemented"



-------------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------------

foreign import ccall "aPrimEval"      eval#             :: String# -> IO Any#

-- |
-- Evaluates the given string as JavaScript.
--
-- /ECMA-262 15.1.2.1/
--
eval :: JsVal a => JsString -> IO a
eval = unsafeCoerce . eval# . getJsString


foreign import ccall "aPrimWrite"     documentWrite#    :: String# -> IO ()
foreign import ccall "aPrimLog"       consoleLog#       :: String# -> IO ()
foreign import ccall "aPrimAlert"     alert#            :: String# -> IO ()

-- |
-- Displays a modal window with the given text.
alert :: String -> IO ()
alert str  = alert# (toJsString# $ str)

-- |
-- Posts a line to the web console.
printLog :: String -> IO ()
printLog str = consoleLog# (toJsString# $ str)

-- |
-- Appends the given content at the end of the `body` element.
printDoc :: String -> IO ()
printDoc str = documentWrite# (toJsString# $ str)

-- |
-- Activates the JavaScript debugger.
--
-- /ECMA-262 12.15/
debug :: IO ()
debug = eval "debugger"
{-# NOINLINE debug #-}


foreign import ccall "aPrimLog"       printRepr#        :: Any#    -> IO ()

-- |
-- Prints the JavaScript representation of the given Haskell value.                  
--
-- The representation of an arbitrary object is should not be relied upon. However, it
-- may be useful in certain situations (such as reading JavaScript error messages).
--
printRepr :: a -> IO ()
printRepr = printRepr# . unsafeCoerce . toPtr#
{-# NOINLINE printRepr #-}

