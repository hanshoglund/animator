
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings, 
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

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

module Animator.Internal.Prim (

        -- ------------------------------------------------------------
        -- ** JavaScript type classes
        -- *** All types
        JsVal( typeOf ),

        -- *** Reference types
        JsRef(..),

        -- *** Sequence types
        JsSeq(..),

        -- ------------------------------------------------------------
        -- ** Objects
        JsName,
        JsObject,

        -- *** Creation and access
        object,
        create,
        global,
        null,

        -- *** Properties
        get,
        set,
        update,
        delete,
        hasProperty,
        hasOwnProperty,
        propertyIsEnumerable,
        lookup,
        (%%),

        -- *** Prototype hierarchy
        -- $proto
        isInstanceOf,
        isPrototypeOf,
        constructor,
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

        -- *** Manipulation
        reverse,
        sort,
        copy,
        take,
        drop,
        slice,
        
        -- *** Array to string
        -- $stringToArray
        join,

        -- ------------------------------------------------------------
        -- ** Strings
        JsString,
        toJsString,
        fromJsString,

        -- *** Creation and access
        fromCharCode,
        charAt,
        charCodeAt,

        -- *** Searching
        indexOf,
        lastIndexOf,
        -- match
        -- replace
        -- search   
        
        -- *** Manipulation and conversion
        sliceString,
        toLower,
        toUpper,

        -- *** String to array
        -- $stringToArray
        split,

        -- ------------------------------------------------------------
        -- ** Functions
        JsFunction,
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

import Prelude hiding (reverse, null, length, lookup, take, drop)

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

-- |
-- Class of JavaScript types.
--
class JsVal a where            
    toAny# :: a -> Any#
    toAny#  = unsafeCoerce

    fromAny# :: Any# -> a
    fromAny# = unsafeCoerce
    
    get# :: JsObject -> JsName -> IO a
    set# :: JsObject -> JsName -> a -> IO ()
    
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

foreign import ccall "aPrimTypeOf" typeOf# :: Any# -> String#

foreign import ccall "aPrimHas"    has#         :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimDelete" delete#      :: Int -> Any# -> String# -> IO Bool
                                                
foreign import ccall "aPrimGet"    getBool#     :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimGet"    getInt#      :: Int -> Any# -> String# -> IO Int
foreign import ccall "aPrimGet"    getWord#     :: Int -> Any# -> String# -> IO Word
foreign import ccall "aPrimGet"    getDouble#   :: Int -> Any# -> String# -> IO Double
foreign import ccall "aPrimGet"    getString#   :: Int -> Any# -> String# -> IO String#
foreign import ccall "aPrimGet"    getAny#      :: Int -> Any# -> String# -> IO Any#
                                                
foreign import ccall "aPrimSet"    setBool#     :: Int -> Any# -> String# -> Bool    -> IO ()
foreign import ccall "aPrimSet"    setInt#      :: Int -> Any# -> String# -> Int     -> IO ()
foreign import ccall "aPrimSet"    setWord#     :: Int -> Any# -> String# -> Word    -> IO ()
foreign import ccall "aPrimSet"    setDouble#   :: Int -> Any# -> String# -> Double  -> IO ()
foreign import ccall "aPrimSet"    setString#   :: Int -> Any# -> String# -> String# -> IO ()
foreign import ccall "aPrimSet"    setAny#      :: Int -> Any# -> String# -> Any#    -> IO ()

mkGet# i c x n   = i (getJsObject x) (toJsString# n)  >>= (return . c)
mkSet# o c x n v = o (getJsObject x) (toJsString# n) (c v)

numberType#    = 0
stringType#    = 1
objectType#    = 2
functionType#  = 3
booleanType#   = 4    

-- In JavaScript @undefined@ is really (), not _|_
--
-- The result of the void operator, the debugger statement and functions without return 
-- values is @undefined@, which should be IO () in Haskell.
instance JsVal () where
    typeOf ()   = "undefined"
    get# _ _    = return ()
    set# _ _ () = return ()
    -- TODO what happens if undefined is passed or returned?
instance JsVal Bool where
    typeOf _    = "boolean"
    get#        = mkGet# (getBool# booleanType#) id
    set#        = mkSet# (setBool# booleanType#) id
    toAny#       = unsafeCoerce . toPtr#
    fromAny#     = unsafeCoerce . fromPtr#
instance JsVal Int where
    typeOf _    = "number"
    get#        = mkGet# (getInt# numberType#) id
    set#        = mkSet# (setInt# numberType#) id
instance JsVal Word where
    typeOf _    = "number"
    get#        = mkGet# (getWord# numberType#) id
    set#        = mkSet# (setWord# numberType#) id
instance JsVal Double where
    typeOf _    = "number"
    get#        = mkGet# (getDouble# numberType#) id
    set#        = mkSet# (setDouble# numberType#) id
instance JsVal JsString where
    typeOf _    = "string"
    get#        = mkGet# (getString# stringType#) JsString
    set#        = mkSet# (setString# stringType#) getJsString
instance JsVal JsObject where
    typeOf      = JsString . typeOf# . getJsObject
    get#        = mkGet# (getAny# objectType#) JsObject
    set#        = mkSet# (setAny# objectType#) getJsObject
instance JsVal JsArray where
    typeOf _    = "object"
    get#        = mkGet# (getAny# objectType#) JsArray
    set#        = mkSet# (setAny# objectType#) getJsArray
instance JsVal JsFunction where
    typeOf _    = "function"
    get#        = mkGet# (getAny# functionType#) JsFunction
    set#        = mkSet# (setAny# functionType#) getJsFunction
instance JsVal (Ptr a) where
    typeOf _    = "object"
    get#        = mkGet# (getAny# objectType#) unsafeCoerce -- TODO problem?
    set#        = mkSet# (setAny# objectType#) unsafeCoerce

-- |
-- Class of JavaScript reference types.
--
class JsVal a => JsRef a where
    toObject :: a -> JsObject
instance JsRef JsObject where
    toObject = id
instance JsRef JsArray where
    toObject = JsObject . getJsArray
instance JsRef JsFunction where
    toObject = JsObject . getJsFunction
instance JsRef JsString where
    toObject = JsObject . unsafeCoerce . getJsString

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


-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

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
-- Fetch the value of property @n@ in object @o@, or equivalently
--
-- > o.n
get :: JsVal a => JsObject -> JsName -> IO a
get = get#

-- | 
-- Assign the property @n@ to @x@ in object @o@, or equivalently
--
-- > o.n = x
set :: JsVal a => JsObject -> JsName -> a -> IO ()
set = set#

-- | 
-- Updates the value named @n@ in object @o@ by applying the function f, or equivalently
--
-- > x.n = f(x.n)
update :: (JsVal a, JsVal b) => JsObject -> JsName -> (a -> b) -> IO ()
update n o f = get n o >>= set n o . f  

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
-- > "n" in o
--
hasProperty :: JsObject -> JsName -> IO Bool
hasProperty x n = has# 0 (getJsObject x) (toJsString# n)

-- | 
-- Recursively traverse an object hierarchy using 'get'.
--   
-- @lookup o [a1,a2, ... an]@ is equivalent to
--
-- > o.a1.a2. ... an
lookup :: JsVal a => JsObject -> [JsName] -> IO a
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

foreign import ccall "aPrimNull"       null#             :: Any#
foreign import ccall "aPrimObj"        object#           :: IO Any#
foreign import ccall "aPrimGlobal"     global#           :: IO Any#
foreign import ccall "aPrimInstanceOf" instanceOf#       :: Any# -> Any# -> Bool

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
create = unsafeLookup ["Object"] %. "create"

-------------------------------------------------------------------------------------
-- 
-- $proto
-- These functions allow introspection of the prototype hierarchy. Note that allthough
-- some implementations allow mutation of the prototype, this behavior is not supported
-- by the standard and should not be used.
--

-- |
-- Return true if object @x@ is an instance created by @y@, or equivalently
--
-- > x instanceof y
--
isInstanceOf :: JsObject -> JsObject -> Bool
x `isInstanceOf` y = p x `instanceOf#` p y 
    where p = getJsObject

-- |
-- Return true if object @x@ is the prototype of object @y@, or equivalently
--
-- > x.isPrototypeOf(y)
-- 
isPrototypeOf :: JsObject -> JsObject -> Bool
x `isPrototypeOf` y = unsafePerformIO (x %. "isPrototypeOf" $ y)
    
-- |
-- Returns the constructor of object @x@, or equivalently
--
-- > x.constructor
--
constructor :: JsObject -> JsFunction
constructor x = unsafePerformIO (x %% "constructor")

-------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------

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


-------------------------------------------------------------------------------------
-- Arrays
-------------------------------------------------------------------------------------

-- |
-- A JavaScript array.
--
--  /ECMA-262 15.4/
--
newtype JsArray = JsArray { getJsArray :: Any# }

foreign import ccall "aPrimArr"    array#   :: IO Any#

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
-- > x.length
--
length :: JsArray -> IO Int
length x = toObject x %% "length"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.push(x)
--
push :: JsVal a => JsArray -> a -> IO JsArray
push x = toObject x %. "push"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.pop()
--
pop :: JsVal a => JsArray -> IO a
pop x = toObject x % "pop"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.shift()
--
shift :: JsVal a => JsArray -> IO a
shift x = toObject x % "shift"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.shift(x)
--
unshift :: JsVal a => JsArray -> a -> IO JsArray
unshift x = toObject x %. "unshift"

-- |
-- Returns
--
-- > x.reverse()
--
reverse :: JsArray -> IO JsArray
reverse x = toObject x % "reverse"

-- |
-- Returns
--
-- > x.sort()
--
sort :: JsArray -> IO JsArray
sort x = toObject x % "sort"

-- splice

-- |
-- Returns
--
-- > x.slice()
--
copy :: JsArray -> IO JsArray
copy x = drop x 0

-- |
-- Returns
--
-- > x.slice(0,a)
--
take :: JsArray -> Int -> IO JsArray
take x = slice x 0

-- |
-- Returns
--
-- > x.slice(a)
--
drop :: JsArray -> Int -> IO JsArray
drop x = toObject x %. "slice"

-- |
-- Returns
--
-- > x.slice(a, b)
--
slice :: JsArray -> Int -> Int -> IO JsArray
slice x a b = (toObject x %.. "slice") a b

--
-- $stringToArray
--
-- The following law holds for all strings @s@ and @t@
--
-- > join t . split t = id
-- > split t . join t = id
--

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.join(s)
--   
join :: JsString -> JsArray -> IO JsString
join = flip join'
    where
        join' x = toObject x %. "join"

-- |
-- Convert an array of (singleton) strings to a string
--   
fromArray :: JsArray -> IO JsString
fromArray = join ""

-- |
-- Convert aa string to an array of (singleton) strings
--   
fromString :: JsString -> JsArray
fromString = split ""


-------------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------------

foreign import ccall "aPrimAdd" concatString# :: String# -> String# -> String#

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
-- Returns
--
-- > String.fromCharCode(a)
--
fromCharCode :: Int -> JsString
fromCharCode = unsafeLookup ["String"] &. "fromCharCode"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > x.charAt(a)
--
charAt :: JsString -> Int -> JsString
charAt x = toObject x &. "charAt"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > x.charCodeAt(a)
--
charCodeAt :: JsString -> Int -> Int
charCodeAt x = toObject x &. "charCodeAt"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.indexOf.call(s, c)
--
indexOf :: JsString -> JsString -> Int
indexOf x = toObject x &. "indexOf"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.lastIndexOf.call(s, c)
--
lastIndexOf :: JsString -> JsString -> Int
lastIndexOf x = toObject x &. "lastIndexOf"

-- match
-- replace
-- search

-- |
-- Returns
--
-- > x.split(t)
--
split :: JsString -> JsString -> JsArray
split = flip split'
    where
        split' x = toObject x &. "split"

-- |
-- Returns
--
-- > x.slice(a, b)
--
sliceString :: JsString -> Int -> Int -> JsString
sliceString x a b = (toObject x &.. "slice") a b

-- |
-- Returns
--
-- > x.toLower()
--
toLower :: JsString -> JsString
toLower x = toObject x & "toLower"

-- |
-- Returns
--
-- > x.toUpper()
--
toUpper :: JsString -> JsString
toUpper x = toObject x & "toUpper"


-------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------

-- |
-- A JavaScript function, i.e. a callable object.
--
-- This type is disjoint from ordinary Haskell functions, which have a compiler-specific
-- internal representation. To convert between the two, use 'call', 'lift' or 'liftPure'.
--
-- /ECMA-262 9.11, 15.3/
--
newtype JsFunction = JsFunction { getJsFunction :: Any# }

-- |
-- Returns the arity of the given function, or equivalently
--
-- > f.length
--
arity :: JsFunction -> Int
arity x = unsafePerformIO $ (toObject x %% "length")

foreign import ccall "aPrimCall0" call#  :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall1" call1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall2" call2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#

-- |
-- Apply the given function, or equivalently
--
-- > f()
--
call :: JsVal a => JsFunction -> IO a

-- |
-- Apply the given function, or equivalently
--
-- > f(a)
--
call1 :: (JsVal a, JsVal b) => JsFunction -> a -> IO b

-- |
-- Apply the given function, or equivalently
--
-- > f(a, b)
--
call2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> IO c

call  f = callWith  f null
call1 f = callWith1 f null
call2 f = callWith2 f null

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg)
--
callWith :: JsVal a => JsFunction -> JsObject -> IO a

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a)
--
callWith1 :: (JsVal a, JsVal b) => JsFunction -> JsObject -> a -> IO b

-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg, a, b)
--
callWith2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> JsObject -> a -> b -> IO c

callWith f t = do
    r <- call# (getJsFunction f) (p t)
    return $ q r
    where
        p = toAny#
        q = fromAny#

callWith1 f t a = do
    r <- call1# (getJsFunction f) (p t) (p a)
    return $ q r
    where
        p = toAny#
        q = fromAny#

callWith2 f t a b = do
    r <- call2# (getJsFunction f) (p t) (p a) (p b)
    return $ q r
    where
        p = toAny#
        q = fromAny#


foreign import ccall "aPrimBind0" bind#  :: Any# -> Any# -> Any#
foreign import ccall "aPrimBind1" bind1#  :: Any# -> Any# -> Any# -> Any#

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a)
--
bind :: JsVal a => JsFunction -> a -> JsFunction
bind  f = bindWith1 f null

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg)
--
bindWith :: JsFunction -> JsObject -> JsFunction

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg, a)
--
bindWith1 :: JsVal a => JsFunction -> JsObject -> a -> JsFunction

bindWith f t = JsFunction $ bind# (getJsFunction f) (p t)
    where
        p = toAny#

bindWith1 f t a = JsFunction $ bind1# (getJsFunction f) (p t) (p a)
    where
        p = toAny#


infixl 9 &
infixl 9 &.
infixl 9 &..
infixl 9 %
infixl 9 %.
infixl 9 %..
infixl 9 %%

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
-- Infix version of 'invokePure'.
--
(&)   = invokePure

-- |
-- Infix version of 'invokePure1'.
--
(&.)  = invokePure1

-- |
-- Infix version of 'invokePure2'.
--
(&..) = invokePure2

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

invoke o n = do
    f <- get o n
    callWith f o

invoke1 o n a = do
    f <- get o n
    callWith1 f o a

invoke2 o n a b = do
    f <- get o n
    callWith2 f o a b

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n()
--
invokePure :: JsVal a => JsObject -> JsName -> a

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a)
--
invokePure1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> b

-- |
-- Invoke the method of the given name on the given object, or equivalently
--
-- > o.n(a, b)
--
invokePure2 :: (JsVal a, JsVal b, JsVal c) => JsObject -> JsName -> a -> b -> c

invokePure  o n     = unsafePerformIO $ invoke  o n
invokePure1 o n a   = unsafePerformIO $ invoke1 o n a
invokePure2 o n a b = unsafePerformIO $ invoke2 o n a b


foreign import ccall "aPrimLiftPure0" liftPure#   :: Any# -> Any#
foreign import ccall "aPrimLiftPure1" liftPure1#  :: Any# -> Any#
foreign import ccall "aPrimLiftPure2" liftPure2#  :: Any# -> Any#
foreign import ccall "aPrimLift0" lift#   :: Any# -> Any#
foreign import ccall "aPrimLift1" lift1#  :: Any# -> Any#
foreign import ccall "aPrimLift2" lift2#  :: Any# -> Any#

liftPure :: JsVal a => a -> JsFunction
liftPure1 :: (JsVal a, JsVal b) => (a -> b) -> JsFunction
liftPure2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> c) -> JsFunction

lift :: JsVal a => IO a -> JsFunction
lift1 :: (JsVal a, JsVal b) => (a -> IO b) -> JsFunction
lift2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> IO c) -> JsFunction


liftPure  = JsFunction . liftPure#  . unsafeCoerce . toPtr#
liftPure1 = JsFunction . liftPure1# . unsafeCoerce . toPtr#
liftPure2 = JsFunction . liftPure2# . unsafeCoerce . toPtr#
lift      = JsFunction . lift#  . unsafeCoerce . toPtr#
lift1     = JsFunction . lift1# . unsafeCoerce . toPtr#
lift2     = JsFunction . lift2# . unsafeCoerce . toPtr#


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

foreign import ccall "aPrimEval"  eval#  :: String# -> IO Any#
foreign import ccall "aPrimDebug" debug# :: IO ()

-- |
-- Evaluates the given string as JavaScript.
--
-- /ECMA-262 15.1.2.1/
--
eval :: JsVal a => JsString -> IO a
eval = fmap fromAny# . eval# . getJsString

-------------------------------------------------------------------------------------

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
debug = debug#

-------------------------------------------------------------------------------------

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

