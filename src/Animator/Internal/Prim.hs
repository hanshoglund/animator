
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings, 
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction,
    FlexibleInstances #-}

-------------------------------------------------------------------------------------

-- |
-- The "Animator.Internal.Prim" modules provide a dynamic interface to the JavaScript host environment.
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

        -- ------------------------------------------------------------
        -- ** JavaScript type classes
        -- *** All types
        JsVal( typeOf ),

        -- *** Reference types
        JsRef(..),

        -- *** Sequence types
        JsSeq(..),

        -- *** Property types
        JsProp(..),
        JsName,
        lookup,

        --- **** Infix version
        (%%),

        -- ------------------------------------------------------------
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

        -- ------------------------------------------------------------
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

        -- ------------------------------------------------------------
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

        -- ------------------------------------------------------------
        -- ** Functions
        JsFun,
        arity,
        call,
        call1,
        call2,
        
        -- *** Partial application
        bind,

        -- *** Lifting Haskell functions
        lift,
        lift1,
        lift2,
        liftPure,
        liftPure1,
        liftPure2,

        -- *** Method invokcation
        invoke,
        invoke1,
        invoke2,

        -- *** With explicit 'this' argument
        bindWith,

        -- *** Infix versions
        (%),
        (%.),
        (%..),
        -- apply,
        -- new,

        -- ------------------------------------------------------------
        -- ** JSON
        JSON,
        parse,
        stringify,

        -- ------------------------------------------------------------
        -- ** Utility
        -- *** Print
        alert,
        printLog,
        printDoc,

        -- *** Eval
        eval,

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
import Haste.Prim(Ptr(..))

#endif __HASTE__

import Foreign.Ptr(Ptr)

#ifdef __HASTE__

type Any#     = H.JSAny              -- Opaque reference
type String#  = H.JSString
type JSON#    = HJ.JSON
toJsString#   = H.toJSStr
fromJsString# = H.fromJSStr
toPtr#        = H.toPtr
fromPtr#      = H.fromPtr

#else

type Any#     = Ptr Int
type String#  = Int
type JSON#    = Int
toJsString#   = undefined
fromJsString# = undefined
toPtr#        = undefined
fromPtr#      = undefined

#endif __HASTE__


foreign import ccall "aPrimTypeOf" typeOf# :: Any# -> String#

-- |
-- Class of JavaScript types.
--
class JsVal a where
            
    toAny :: a -> Any#
    toAny = unsafeCoerce

    fromAny :: Any# -> a
    fromAny = unsafeCoerce
    
    -- | 
    -- Returns a string describing a type of the given value, or equivalently
    --
    -- > typeof x
    --
    -- By definition, this function returns one of the following strings:
    --
    -- > "undefined", "boolean", "number", "string", "object", "function"
    -- 
    typeOf :: JsVal a => a -> JsString

-- In JavaScript undefined is really (), not _|_
--
-- The result of the void operator, the debugger statement and functions without return 
-- values is 'undefined', which should be IO () in Haskell.
instance JsVal () where
    typeOf () = "undefined"

instance JsVal Bool where
    toAny   = unsafeCoerce . toPtr#
    fromAny = unsafeCoerce . fromPtr#
    typeOf _ = "boolean"
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
    typeOf = JsString . typeOf# . getJsObject
instance JsVal JsArray where
    typeOf _ = "object"
instance JsVal (JsFun a) where
    typeOf _ = "function"
instance JsVal (Ptr a) where
    typeOf _ = "object"


-- |
-- Class of JavaScript reference types.
--
class JsVal a => JsRef a where
    toObject :: a -> JsObject
instance JsRef (JsFun a) where
    toObject = JsObject . getJsFun
instance JsRef JsArray where
    toObject = JsObject . getJsArray
instance JsRef JsObject where
    toObject = id

-- |
-- Class of JavaScript sequence types.
--
class JsRef a => JsSeq a where
    toArray :: a -> JsArray
instance JsSeq JsArray where
    toArray = id


-- |
-- A JavaScript property name.
--
type JsName = String

-- |
-- Class of types that can be properties of a 'JsObject'.
--
-- Retrieving a value of the wrong type (for example, reading an 'Int' from a field
-- containing a string) results in a runtime error.
--
class JsVal a => JsProp a where
    -- | Fetch the value of property @n@ in object @o@, or equivalently
    --
    -- > o.n
    get :: JsObject -> JsName -> IO a

    -- | Assign the property @n@ to @x@ in object @o@, or equivalently
    --
    -- > o.n = x
    set :: JsObject -> JsName -> a -> IO ()

    -- | Updates the value named @n@ in object @o@ by applying the function f,
    --   or equivalently
    --
    -- > x.n = f(x.n)
    update :: JsObject -> JsName -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f

-- | 
-- Recursively traverse an object hierarchy using 'get'.
--   
-- @lookup o [a1,a2, ... an]@ is equivalent to
--
-- > o.a1.a2. ... an
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

unsafeGlobal = unsafePerformIO $ global
unsafeLookup = unsafePerformIO . lookup unsafeGlobal


-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

foreign import ccall "aPrimNull"       null#             :: Any#
foreign import ccall "aPrimObj"        object#           :: IO Any#
foreign import ccall "aPrimGlobal"     global#           :: IO Any#
foreign import ccall "aPrimInstanceOf" instanceOf#       :: Any# -> Any# -> Bool

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
-- Return the JavaScript null object, or equivalently
--
-- > null
--
null :: JsObject
null = JsObject $ null#

-- |
-- Create a new JavaScript object, or equivalently
--
-- > {}
--
object :: IO JsObject
object = object# >>= (return . JsObject)

-- |
-- Return the JavaScript global object, or equivalently
--
-- > window
--
global :: IO JsObject
global = global# >>= (return . JsObject)

-- |
-- Create a new JavaScript object using the given object as prototype, or equivalently
--
-- > Object.create(x)
--
--
create :: JsObject -> IO JsObject
create = (unsafeLookup ["Object"]) %. "create"

-- |
-- Return true if object @x@ is an instance created by @y@, or equivalently
--
-- > x instanceof y
--
-- This function is pure, as the prototype of an object is not supposed to
-- change, at least not as far as the standard is concerned.
--
isInstanceOf :: JsObject -> JsObject -> Bool
x `isInstanceOf` y = p x `instanceOf#` p y 
    where p = getJsObject

-- |
-- Return true if object @x@ is the prototype of object @y@, or equivalently
--
-- > x.isPrototypeOf(y)
-- 
-- This function is pure, as the prototype of an object is not supposed to
-- change, at least not as far as the standard is concerned.
-- 
isPrototypeOf :: JsObject -> JsObject -> Bool
x `isPrototypeOf` y = unsafePerformIO (x %. "isPrototypeOf" $ y)
    
-- |
-- Returns the constructor of object @x@, or equivalently
--
-- > x.constructor
--
-- This function is pure, as the constructor of an object is not supposed to
-- change.
--
constructor :: JsObject -> JsFun a
constructor x = unsafePerformIO (x %% "constructor")

-- |
-- Deletes the property @n@ form object @o@, or equivalently
--
-- > delete o.n
--
delete :: JsObject -> JsName -> IO ()
delete x n = delete# 0 (getJsObject x) (toJsString# n) >> return ()

-- |
-- Returns
--
-- > o.n !== undefined
--
hasProperty :: JsObject -> JsName -> IO Bool
hasProperty x n = has# 0 (getJsObject x) (toJsString# n)

-- |
-- Returns
--
-- > x.hasOwnProperty(y)
--
hasOwnProperty :: JsObject -> JsName -> IO Bool
hasOwnProperty x n = (x %. "hasOwnProperty") (toJsString n)

-- |
-- Returns
--
-- > x.propertyIsEnumerable(y)
--
propertyIsEnumerable :: JsObject -> JsName -> IO Bool
propertyIsEnumerable x n = (x %. "propertyIsEnumerable") (toJsString n)


-- |
-- Returns
--
-- > x.toString(y)
--
toString :: JsObject -> IO JsString
toString x = x % "toString"

-- |
-- Returns
--
-- > x.toLocaleString(y)
--
toLocaleString :: JsObject -> IO JsString
toLocaleString x = x % "toLocaleString"

-- |
-- Returns
--
-- > x.valueOf(y)
--
valueOf :: JsVal a => JsObject -> IO a
valueOf x = x % "valueOf"

foreign import ccall "aPrimHas"       has#              :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimDelete"    delete#           :: Int -> Any# -> String# -> IO Bool

foreign import ccall "aPrimGet"       getBool#          :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimGet"       getInt#           :: Int -> Any# -> String# -> IO Int
foreign import ccall "aPrimGet"       getWord#          :: Int -> Any# -> String# -> IO Word
foreign import ccall "aPrimGet"       getInt32#         :: Int -> Any# -> String# -> IO Int32
foreign import ccall "aPrimGet"       getWord32#        :: Int -> Any# -> String# -> IO Word32
foreign import ccall "aPrimGet"       getFloat#         :: Int -> Any# -> String# -> IO Float
foreign import ccall "aPrimGet"       getDouble#        :: Int -> Any# -> String# -> IO Double
foreign import ccall "aPrimGet"       getString#        :: Int -> Any# -> String# -> IO String#
foreign import ccall "aPrimGet"       getAny#           :: Int -> Any# -> String# -> IO Any#

foreign import ccall "aPrimSet"       setBool#          :: Int -> Any# -> String# -> Bool    -> IO ()
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

numberType#    = 0
stringType#    = 1
objectType#    = 2
functionType#  = 3
booleanType#   = 4

instance JsProp Bool where
    get = get# (getBool# booleanType#) id
    set = set# (setBool# booleanType#) id

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

instance JsProp JsArray where
    get = get# (getAny# objectType#) JsArray
    set = set# (setAny# objectType#) getJsArray

instance JsProp (JsFun a) where
    get = get# (getAny# functionType#) JsFun
    set = set# (setAny# functionType#) getJsFun


-------------------------------------------------------------------------------------
-- Arrays
-------------------------------------------------------------------------------------

foreign import ccall "aPrimArr"       array#            :: IO Any#
foreign import ccall "aPrimArrConcat" concatArray#      :: Any# -> Any# -> Any#

-- |
-- A JavaScript array.
--
-- Informally, arrays are objects with dense numeric indices.
--
--  /ECMA-262 15.4/
--
newtype JsArray = JsArray { getJsArray :: Any# }

-- |
-- Returns
--
-- > []
--
array :: IO JsArray
array = array# >>= (return . JsArray)

-- |
-- Returns the length of the given array, or equivalently
--
-- > xs.length
--
length :: JsArray -> IO Int
length xs = (toObject xs) %% "length"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.push(x)
--
push :: JsProp a => JsArray -> a -> IO JsArray
push xs = (toObject xs) %. "push"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.pop()
--
pop :: JsProp a => JsArray -> IO a
pop xs = (toObject xs) % "pop"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.shift()
--
shift :: JsProp a => JsArray -> IO a
shift xs = (toObject xs) % "shift"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.shift(x)
--
unshift :: JsProp a => JsArray -> a -> IO JsArray
unshift xs = (toObject xs) %. "unshift"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.reverse()
--
reverse :: JsArray -> IO JsArray
reverse xs = (toObject xs) % "reverse"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > xs.sort()
--
sort :: JsArray -> IO JsArray
sort xs = (toObject xs) % "sort"

-- splice

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.join.call(x, s)
--
join :: JsArray -> JsString -> IO JsString
join xs = (toObject xs) %. "join"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.slice.call(x, a, b)
--
sliceArray :: JsArray -> Int -> Int -> IO JsArray
sliceArray xs = (toObject xs) %.. "slice"



-------------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------------

-- |
-- A JavaScript string.
--
-- Informally, a sequence of Unicode characters. Allthough this type is normally used for text, it
-- can be used to store any unsigned 16-bit value using 'charAt' and 'fromCharCode'. Operations on
-- 'JsString' are much more efficient than the equivalent 'String' operations.
--
-- There is no 'Char' type in JavaScript, so functions dealing with single characters return
-- singleton strings.
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
--
toJsString :: String -> JsString
toJsString = JsString . toJsString#

-- |
-- Convert a JavaScript string to a Haskell string.
--
fromJsString :: JsString -> String
fromJsString = fromJsString# . getJsString

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.charAt.call(s, i)
--
charAt :: Int -> JsString -> JsString
charAt = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.indexOf.call(s, c)
--
indexOf :: JsString -> JsString -> Int
indexOf = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.lastIndexOf.call(s, c)
--
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
--
sliceString :: Int -> Int -> JsString -> JsString
sliceString = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.toLowerCase.call(s)
--
toLower :: JsString -> JsString
toLower = error "Not implemented"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.toUpperCase.call(s)
--
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
-- internal representation. To convert between the two, use 'call', 'lift' or 'liftPure'.
--
-- /ECMA-262 9.11, 15.3/
--
newtype JsFun a = JsFun { getJsFun :: Any# }

-- |
-- Returns the arity of the given function, or equivalently
--
-- > f.length
--
arity :: JsFun a -> Int
arity = error "Not implemented"

foreign import ccall "aPrimCall0" call#  :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall1" call1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall2" call2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#

-- |
-- Apply the given function, or equivalently
--
-- > f()
--
call :: JsVal a => JsFun a -> IO a

-- |
-- Apply the given function, or equivalently
--
-- > f(a)
--
call1 :: (JsVal a, JsVal b) => JsFun (a -> b) -> a -> IO b

-- |
-- Apply the given function, or equivalently
--
-- > f(a, b)
--
call2 :: (JsVal a, JsVal b, JsVal c) => JsFun (a -> b -> c) -> a -> b -> IO c

call  f = callWith  f null
call1 f = callWith1 f null
call2 f = callWith2 f null

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg)
--
callWith :: JsVal a => JsFun a -> JsObject -> IO a

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a)
--
callWith1 :: (JsVal a, JsVal b) => JsFun (a -> b) -> JsObject -> a -> IO b

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a, b)
--
callWith2 :: (JsVal a, JsVal b, JsVal c) => JsFun (a -> b -> c) -> JsObject -> a -> b -> IO c

callWith f t = do
    r <- call# (getJsFun f) (p t)
    return $ q r
    where
        p = toAny
        q = fromAny

callWith1 f t a = do
    r <- call1# (getJsFun f) (p t) (p a)
    return $ q r
    where
        p = toAny
        q = fromAny

callWith2 f t a b = do
    r <- call2# (getJsFun f) (p t) (p a) (p b)
    return $ q r
    where
        p = toAny
        q = fromAny


foreign import ccall "aPrimBind0" bind#  :: Any# -> Any# -> Any#
foreign import ccall "aPrimBind1" bind1#  :: Any# -> Any# -> Any# -> Any#

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a)
--
bind :: JsVal a => JsFun (a -> b) -> a -> (JsFun b)
bind  f = bindWith1 f null

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg)
--
bindWith :: JsFun a -> JsObject -> (JsFun a)

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a)
--
bindWith1 :: JsVal a => JsFun (a -> b) -> JsObject -> a -> (JsFun b)

bindWith f t = JsFun $ bind# (getJsFun f) (p t)
    where
        p = toAny

bindWith1 f t a = JsFun $ bind1# (getJsFun f) (p t) (p a)
    where
        p = toAny


infixl 1 %
infixl 1 %.
infixl 1 %..
infixl 1 %%

-- |
-- Infix version of 'invoke'.
--
(%)   = invoke

-- |
-- Infix version of 'invoke1'.
--
(%.)  = invoke1

-- |
-- Infix version of 'invoke2'.
--
(%..) = invoke2

-- |
-- Infix version of 'get'.
--
(%%) = get



-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n()
--
invoke :: JsVal a => JsObject -> JsName -> IO a

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a)
--
invoke1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> IO b

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b)
--
invoke2 :: (JsVal a, JsVal b, JsVal c) => JsObject -> JsName -> a -> b -> IO c

-- -- |
-- -- Invoke the method of the given name on the given object, or equivalently
-- --
-- -- > o.n(a, b, c)
-- invoke3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsObject -> JsName -> a -> b -> c -> IO d
-- 
-- -- |
-- -- Invoke the method of the given name on the given object, or equivalently
-- --
-- -- > o.n(a, b, c, d)
-- invoke4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsObject -> JsName -> a -> b -> c -> d -> IO e
-- 
-- -- |
-- -- Invoke the method of the given name on the given object, or equivalently
-- --
-- -- > o.n(a, b, c, d, e)
-- invoke5 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsObject -> JsName -> a -> b -> c -> d -> e -> IO f

invoke o n = do
    f <- get o n
    callWith f o

invoke1 o n a = do
    f <- get o n
    callWith1 f o a

invoke2 o n a b = do
    f <- get o n
    callWith2 f o a b

-- invoke3 o n a b c = do
--     f <- get o n
--     callWith3 f o a b c
-- 
-- invoke4 o n a b c d = do
--     f <- get o n
--     callWith4 f o a b c d
-- 
-- invoke5 o n a b c d e = do
--     f <- get o n
--     callWith5 f o a b c d e

-- -- |
-- -- Partially apply the given function, or equivalently
-- --
-- -- > Function.prototype.bind.call(f, x, ... as)
-- bind :: JsVal a => JsFun -> a -> [a] -> JsFun
-- bind = error "Not implemented"
--
-- -- |
-- -- Apply the given function, or equivalently
-- --
-- -- > Function.prototype.apply.call(f, x, as)
-- apply :: JsVal a => JsFun -> a -> [a] -> a
-- apply = error "Not implemented"
--
-- -- |
-- -- Invokes the given function as a constructor, or equivalently
-- --
-- -- > new F(args)
-- new :: JsVal a => JsFun -> [a] -> IO JsObject
-- new = error "Not implemented"

foreign import ccall "aPrimLiftPure0" liftPure#   :: Any# -> Any#
foreign import ccall "aPrimLiftPure1" liftPure1#  :: Any# -> Any#
foreign import ccall "aPrimLiftPure2" liftPure2#  :: Any# -> Any#
foreign import ccall "aPrimLift0" lift#   :: Any# -> Any#
foreign import ccall "aPrimLift1" lift1#  :: Any# -> Any#
foreign import ccall "aPrimLift2" lift2#  :: Any# -> Any#

liftPure :: JsVal a => a -> JsFun a
liftPure1 :: (JsVal a, JsVal b) => (a -> b) -> JsFun (a -> b)
liftPure2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> c) -> JsFun (a -> b -> c)

lift :: JsVal a => IO a -> JsFun a
lift1 :: (JsVal a, JsVal b) => (a -> IO b) -> JsFun (a -> b -> c)
lift2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> IO c) -> JsFun (a -> b -> c)


liftPure  = JsFun . liftPure#  . unsafeCoerce . toPtr#
liftPure1 = JsFun . liftPure1# . unsafeCoerce . toPtr#
liftPure2 = JsFun . liftPure2# . unsafeCoerce . toPtr#
lift  = JsFun . lift#  . unsafeCoerce . toPtr#
lift1 = JsFun . lift1# . unsafeCoerce . toPtr#
lift2 = JsFun . lift2# . unsafeCoerce . toPtr#


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

foreign import ccall "aPrimEval" eval#  :: String# -> IO Any#

-- |
-- Evaluates the given string as JavaScript.
--
-- /ECMA-262 15.1.2.1/
--
eval :: JsVal a => JsString -> IO a
eval = fmap fromAny . eval# . getJsString


foreign import ccall "aPrimWrite"     documentWrite#    :: String# -> IO ()
foreign import ccall "aPrimLog"       consoleLog#       :: String# -> IO ()
foreign import ccall "aPrimAlert"     alert#            :: String# -> IO ()

-- |
-- Displays a modal window with the given text.
--
alert :: String -> IO ()
alert str  = alert# (toJsString# $ str)

-- |
-- Posts a line to the web console.
--
printLog :: String -> IO ()
printLog str = consoleLog# (toJsString# $ str)

-- |
-- Appends the given content at the end of the `body` element.
--
printDoc :: String -> IO ()
printDoc str = documentWrite# (toJsString# $ str)

-- |
-- Activates the JavaScript debugger.
--
-- /ECMA-262 12.15/
--
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

