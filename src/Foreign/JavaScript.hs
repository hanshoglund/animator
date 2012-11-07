
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Foreign.JavaScript
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
-- 
-- This module modules provides a dynamic interface to JavaScript. It provides access to
-- all native JavaScript types including strings, arrays, objects and functions which can
-- be manipulated directly in Haskell. JavaScript bjects can be created and updated, 
-- JavaScript functions can be called and so on.
--
-- This flexibility comes at the price of inheriting some of the safety problems of
-- JavaScript, which are outlined below.
--

-------------------------------------------------------------------------------------

module Foreign.JavaScript (

        -- ------------------------------------------------------------

        -- ** JavaScript types
        -- $jsTypeClasses
        
        -- *** All types
        JsVal( typeOf ),

        -- *** Reference types
        JsRef(..),

        -- *** Sequence types
        JsSeq(..),

        -- *** Callable types
        JsCall(..),

        -- ------------------------------------------------------------
        -- ** Objects
        JsObject,

        -- *** Creation and access
        object,
        global,
        create,

        -- *** Prototype hierarchy
        -- $prototypeHierarchy
        prototype,
        constructor,
        isPrototypeOf,
        isInstanceOf,

        -- *** Properties
        JsName,
        get,
        set,
        delete,
        update,
        lookup,
        -- **** Unsafe version
        unsafeGet,
        unsafeLookup,
        unsafeGlobalLookup,

        -- **** Predicates
        hasProperty,
        hasOwnProperty,
        propertyIsEnumerable,
        -- getOwnPropertyNames,
        -- getOwnPropertyDescriptor,

        -- defineProperty,
        -- defineProperties,

        -- seal,
        -- freeze,
        -- preventExtentions,
        -- isSealed,
        -- isFrozen,
        -- isExtensible,

        -- *** Infix versions
        (%?),
        (%),
        (!%),

        -- *** Conversion
        toString,
        toLocaleString,
        valueOf,

        -- ------------------------------------------------------------
        -- ** Arrays
        JsArray,
        -- isArray,

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
        -- splice,
        -- concat,

        -- *** Functional interface
        -- forEach,
        -- map,
        -- filter,
        -- reduceRight,

        -- *** Array to string
        -- $StringToArray
        join,

        -- ------------------------------------------------------------
        -- ** Strings
        JsString,
        toJsString,
        fromJsString,

        -- *** Creation and access
        fromCharCode,
        stringLength,
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
        -- $StringToArray
        split,

        -- ------------------------------------------------------------
        -- ** Functions
        -- $functionLaws
        JsFunction,
        arity,
        
        JsArg(..),

        -- *** Safe
        -- **** Haskell calling JavaScript
        call',
        call,
        call1,
        call2,
        call3,
        call4,

        -- **** JavaScript calling Haskell 
        lift,
        lift1,
        lift2,

        -- **** Object creation
        new',
        new,
        new1,
        new2,
        new3,

        -- **** Method invocation
        invoke,
        invoke1,
        invoke2,

        -- *** Unsafe
        -- **** Haskell calling JavaScript
        unsafeCall,
        unsafeCall1,
        unsafeCall2,

        -- **** JavaScript calling Haskell 
        unsafeLift,
        unsafeLift1,
        unsafeLift2,

        -- **** Method invocation
        unsafeInvoke,
        unsafeInvoke1,
        unsafeInvoke2,

        -- *** Partial application
        bind,
        -- **** With explicit 'this' argument
        bindWith,

        -- *** Infix versions
        (%%),
        (%.),
        (%..),
        (%...),
        (%....),
        (!%%),
        (!%.),
        (!%..),
        (!%...),
        (!%....),
        -- apply,
        -- new,

        -- ------------------------------------------------------------
        -- ** Date
        JsDate,

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

import Prelude hiding (reverse, length, lookup, take, drop)

import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Semigroup
import Control.Applicative (pure, (<*>))

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

toJsString#   :: String -> String#
fromJsString# :: String# -> String
toPtr#   :: a -> Ptr a
fromPtr# :: Ptr a -> a

toJsString#   = H.toJSStr
fromJsString# = H.fromJSStr
toPtr#        = H.toPtr
fromPtr#      = H.fromPtr

#else

type Any#     = Ptr Int
type String#  = Int
type JSON#    = Int

toJsString#   :: String -> String#
fromJsString# :: String# -> String
toPtr#   :: a -> Ptr a
fromPtr# :: Ptr a -> a

toJsString#   = undefined
fromJsString# = undefined
toPtr#        = undefined
fromPtr#      = undefined

#endif __HASTE__



-- $typeSafety
--
-- This module tries to retain full type safety on the Haskell side. However, it is generally
-- impossible to determine the type of a value retrived from JavaScript at runtime, that is
-- when fetching the value of a property, or calling a JavaScript function.
--

-- $purity
--
-- As JavaScript is inherently stateful, most calls take place inside 'IO'.
--
-- However, there are some cases when it is safe to call into JavaScript using only pure
-- functions. One of these is when handling objects known to be immutable (such as strings), or 
-- properties known to be stable (such as the prototype chain). Others are when calling 
-- functions known to be pure (such as 'slice').
--
-- This module provides a set of functions for pure calls for this purpose. By conventions such
-- functions have the suffix /unsafe/.
--

-- $invokeApi
-- 
-- > ffi "A"           ==> unsafeLookupGlobal ["Object"]
-- > ffi "global.a.b"  ==> unsafeLookupGlobal ["a.b"]
-- > ffi "x.foo"       ==> x %   "foo"
-- > ffi "x.foo()"     ==> x %%  "foo"
-- > ffi "x.foo(a)"    ==> (x %.  "foo") a
-- > ffi "x.foo(a,b)"  ==> (x %.. "foo") a b
-- 


-------------------------------------------------------------------------------------

foreign import ccall "aPrimNull"   null#   :: Any#
foreign import ccall "aPrimTypeOf" typeOf# :: Any# -> String#

foreign import ccall "aPrimHas"    has#         :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimDelete" delete#      :: Int -> Any# -> String# -> IO Bool
foreign import ccall "aPrimGet"    getAny#      :: Int -> Any# -> String# -> IO Any#
foreign import ccall "aPrimSet"    setAny#      :: Int -> Any# -> String# -> Any#    -> IO ()

-- TODO simplify?
mkGet# i c x n   = i (getJsObject x) (getJsString n)  >>= (return . c)
mkSet# o c x n v = o (getJsObject x) (getJsString n) (c v)

-- Keep in synch with aPrimTypeCheck
booleanType#   = 0
numberType#    = 1
stringType#    = 2
objectType#    = 3
functionType#  = 4
undefinedType# = 5

-- $jsTypeClasses
--
-- The class 'JsVal' and its subclasses represents types that have a predictable JavaScript
-- representation. The values of these types be passed passed freely between the languages.
--
-- There is no way to make arbitrary Haskell types instances of 'JsVal', as some types have
-- an unspecified representation that should not be relied upon outside Haskell code. 
-- It is however possible  to pass an arbitrary Haskell value to JavaScript by wrapping 
-- it in a pointer.
--
-- Also, a type can be declared an instance of 'JsVal' and its subclasses if /either/
--
--     - It has no constructors, /or/
--
--     - It is a @newtype@ wrapper around a type that is an instance of 'JsVal'
--
-- This idiom is useful for wrapping JavaScript libraries. For example a basic
-- wrapper of the JQuery library looks like this:
--
-- > data Query
-- > instance JsVal Query
-- > instance JsRef Query
-- > instance JsCall Query
-- > 
-- > query :: JsString -> IO Query
-- > query = call1 $ unsafeGlobalLookup ["jQuery"]
-- > 
-- > fadeIn :: Query -> IO ()
-- > fadeIn x = toObject x %% "fadeIn"
-- > 
-- > fadeOut :: Query -> IO ()
-- > fadeOut x = toObject x %% "fadeOut"
--
-- There is no need to provide an implementations of the methods in 'JsVal' or its subclasses.
--
-- To simplify semantics, JavaScript's @undefined@ maps to Haskell's 'undefined', and is thus a member
-- of any type. This means that reading an undefined property, calling a JavaScript method that returns
-- @undefined@, or passing to few arguments when calling a Haskell callback from JavaScript results
-- in a runtime error if the value is used (as do 'undefined' or 'error').
--
-- The only exception to this behaviour is the 'JsVal' instance for @()@, which always map to the unit
-- value. This means that JavaScript functions returning @undefined@ can be imported as @IO ()@.
--

-- |
-- Class of JavaScript types.
--
class JsVal a where
    toAny# :: a -> Any#
    toAny#  = unsafeCoerce

    fromAny# :: Any# -> a
    fromAny# = unsafeCoerce
    
    typeId# :: a -> Int
    typeId# _ = objectType#

    get# :: JsObject -> JsName -> IO a
    get# w = mkGet# (getAny# $ typeId# w) fromAny# $ w

    set# :: JsObject -> JsName -> a -> IO ()
    set# w = mkSet# (setAny# $ typeId# w) toAny# $ w

    -- |
    -- Returns a string describing a type of the given value, or equivalently
    --
    -- > typeof x
    --
    -- By definition, this function returns one of the following strings:
    --
    -- > "undefined", "boolean", "number", "string", "object", "function"
    --
    typeOf :: a -> JsString
    typeOf = JsString . typeOf# . toAny#

-- | Represented by @null@.
instance JsVal () where
    typeId# _   = objectType#
    fromAny# _  = ()
    toAny# _    = null#
    get# _ _    = return ()
    set#        = mkSet# (setAny# $ typeId# ()) (const null#)

-- | Represented by @boolean@ values.
instance JsVal Bool where
    typeId# _   = booleanType#
    toAny#      = unsafeCoerce . toPtr#
    fromAny#    = unsafeCoerce . fromPtr#

-- | Represented by @number@ values.
instance JsVal Int where
    typeId# _   = numberType#

-- | Represented by @number@ values.
instance JsVal Word where
    typeId# _   = numberType#

-- | Represented by @number@ values.
instance JsVal Int32 where
    typeId# _   = numberType#

-- | Represented by @number@ values.
instance JsVal Word32 where
    typeId# _   = numberType#

-- | Represented by @number@ values.
instance JsVal Float where
    typeId# _   = numberType#

-- | Represented by @number@ values.
instance JsVal Double where
    typeId# _   = numberType#

-- | Represented by @string@ values.
instance JsVal JsString where
    typeId# _   = stringType#

-- | Represented by @object@ values whose prototype chain include @Object.prototype@.
instance JsVal JsObject where
    typeId# _   = objectType#

-- | Represented by @object@ values whose prototype chain include @Array.prototype@.
instance JsVal JsArray where
    typeId# _   = objectType#

-- | Represented by @object@ values whose prototype chain include @Date.prototype@.
instance JsVal JsDate where
    typeId# _   = objectType#

-- | Represented by @function@ values whose prototype chain include @Function.prototype@.
instance JsVal JsFunction where
    typeId# _   = functionType#

-- | Represention is opaque.
instance JsVal (Ptr a) where
    typeId# _   = objectType#

-- |
-- Class of JavaScript reference types.
--
class JsVal a => JsRef a where
    -- | Cast an object descended from @Object.prototype@.
    toObject :: a -> JsObject
    toObject = unsafeCoerce

instance JsRef () where -- TODO wrong?
instance JsRef JsString where
instance JsRef JsObject where
instance JsRef JsArray where
instance JsRef JsDate where
instance JsRef JsFunction where

-- |
-- Class of JavaScript sequence types.
--
class JsRef a => JsSeq a where
    -- | Cast an object descended from @Array.prototype@.
    toArray :: a -> JsArray
    toArray = unsafeCoerce
instance JsSeq JsArray where

-- |
-- Class of JavaScript callable types.
--
class JsRef a => JsCall a where
    -- | Cast a callable object descended from @Function.prototype@.
    toFunction :: a -> JsFunction
    toFunction = unsafeCoerce
instance JsCall JsFunction where


-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

-- |
-- A JavaScript object.
--
--  /ECMA-262 8.6, 15.2/
--
newtype JsObject = JsObject { getJsObject :: Any# }


-- |
-- A JavaScript property name, or object key.
--
type JsName = JsString

-- |
-- Return the value of property @n@ in object @x@, or equivalently
--
-- > x.n
get :: JsVal a => JsObject -> JsName -> IO a
get = get#

-- |
-- Assign the property @n@ to @a@ in object @x@, or equivalently
--
-- > x.n = a
--
set :: JsVal a => JsObject -> JsName -> a -> IO ()
set = set#

-- |
-- Updates the value named @n@ in object @x@ by applying @f@, or equivalently
--
-- > x.n = f(x.n)
update :: (JsVal a, JsVal b) => JsObject -> JsName -> (a -> b) -> IO ()
update n o f = get n o >>= set n o . f

-- |
-- Deletes the property @n@ form object @x@, or equivalently
--
-- > delete x.n
--
delete :: JsObject -> JsName -> IO ()
delete x n = delete# 0 (getJsObject x) (getJsString n) >> return ()

-- |
-- Returns true if property @n@ is defined in object @x@.
-- 
-- The following law holds for all @x@ and @n@
--
-- > x `hasProperty` n = fmap isDefined (x % n)
--
hasProperty :: JsObject -> JsName -> IO Bool
hasProperty x n = has# 0 (getJsObject x) (getJsString n)

-- |
-- Recursively lookup a value in an object structure.
--
-- @lookup x [n1,n2, ... nj]@ is equivalent to
--
-- > o.n1.n2. ... nj
lookup :: JsVal a => JsObject -> [JsName] -> IO a
lookup o [] = error "lookup: Empty list"
lookup o (x:xs) = do
    o' <- lookup' o xs
    get o' x

lookup' :: JsObject -> [JsName] -> IO JsObject
lookup' o [] = return $ o
lookup' o (x:xs) = do
    o' <- lookup' o xs
    get o' x

-- |
-- Fetch the value of the property @n@ in object @x@, or equivalently
--
-- > o.n
--
-- This unsafe function assumes that the given property exits and refers to a stable value.
-- 
unsafeGet :: JsVal a => JsObject -> JsName -> a
unsafeGet o = unsafePerformIO . get o

-- |
-- Recursively lookup a value in an object structure.
--
-- @unsafeLookup o [a1,a2, ... an]@ is equivalent to
--
-- > o.a1.a2. ... an
--
-- This unsafe function assumes that the given property exits and refers to a stable value.
--
unsafeLookup :: JsVal c => JsObject -> [JsName] -> c
unsafeLookup o = unsafePerformIO . lookup o

-- |
-- Recursively lookup a global value. This function is useful for calling static
-- functions such as @Object.create@.
--
-- > create :: JsObject -> IO JsObject
-- > create = unsafeGlobalLookup ["Object"] %. "create"
--
-- This unsafe function assumes that the given property exits and refers to a stable value.
--
unsafeGlobalLookup :: JsVal c => [JsName] -> c
unsafeGlobalLookup = unsafeLookup g
    where
        g = unsafePerformIO $ global


-------------------------------------------------------------------------------------

foreign import ccall "aPrimObj"        object#           :: IO Any#
foreign import ccall "aPrimGlobal"     global#           :: IO Any#
foreign import ccall "aPrimInstanceOf" instanceOf#       :: Any# -> Any# -> Bool

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

-- withGlobal :: (JsObject -> IO a) -> IO a
-- withGlobal f = global >>= f


-- |
-- Create a new JavaScript object using the given object as prototype, or equivalently
--
-- > Object.create(x)
--
--
create :: JsObject -> IO JsObject
create = unsafeGlobalLookup ["Object"] %. "create"

-------------------------------------------------------------------------------------
--
-- $prototypeHierarchy
--
-- These functions allow introspection of the prototype hierarchy. They are pure as the prototype
-- chain is immutable, at least as far as the standard is concerned.
--

-- |
-- Returns the prototype of object @x@, or equivalently
--
-- > Object.getPrototypeOf(x)
--
prototype :: JsObject -> JsObject
prototype = unsafeGlobalLookup ["Object"] !%. "getPrototytpeOf"

-- |
-- Returns the constructor of object @x@, or equivalently
--
-- > x.constructor
--
constructor :: JsObject -> JsFunction
constructor x = x !% "constructor"

-- |
-- Return true if object @x@ is the prototype of object @y@, or equivalently
--
-- > x.isPrototypeOf(y)
--
isPrototypeOf :: JsObject -> JsObject -> Bool
x `isPrototypeOf` y = x !%. "isPrototypeOf" $ y

-- |
-- Return true if object @x@ is an instance created by @y@, or equivalently
--
-- > x instanceof y
--
isInstanceOf :: JsObject -> JsFunction -> Bool
x `isInstanceOf` y = p x `instanceOf#` q y
    where
        p = getJsObject
        q = getJsFunction


-------------------------------------------------------------------------------------

-- |
-- Returns
--
-- > x.hasOwnProperty(y)
--
hasOwnProperty :: JsObject -> JsName -> IO Bool
hasOwnProperty x n = (x %. "hasOwnProperty") n

-- |
-- Returns
--
-- > x.propertyIsEnumerable(y)
--
propertyIsEnumerable :: JsObject -> JsName -> IO Bool
propertyIsEnumerable x n = (x %. "propertyIsEnumerable") n

-------------------------------------------------------------------------------------

-- |
-- Returns
--
-- > x.toString()
--
toString :: JsObject -> IO JsString
toString x = x %% "toString"

-- |
-- Returns
--
-- > x.toLocaleString()
--
toLocaleString :: JsObject -> IO JsString
toLocaleString x = x %% "toLocaleString"

-- |
-- Returns
--
-- > x.valueOf()
--
valueOf :: JsVal a => JsObject -> IO a
valueOf x = x %% "valueOf"


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
length x = toObject x % "length"

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
pop x = toObject x %% "pop"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > x.shift()
--
shift :: JsVal a => JsArray -> IO a
shift x = toObject x %% "shift"

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
reverse x = toObject x %% "reverse"

-- |
-- Returns
--
-- > x.sort()
--
sort :: JsArray -> IO JsArray
sort x = toObject x %% "sort"

-- splice

-- |
-- Returns a copy of @x@.
--
-- > x.slice()
--
copy :: JsArray -> IO JsArray
copy x = drop x 0

-- |
-- Returns the array @[ x[0],x[1] ... x[a] ]@, or equivalently
--
-- > x.slice(0,a)
--
take :: JsArray -> Int -> IO JsArray
take x = slice x 0

-- |
-- Returns the array @[ x[a],x[a+1] ... x[n] ]@, where @n@ is the length ox @x@, or equivalently
--
-- > x.slice(a)
--
drop :: JsArray -> Int -> IO JsArray
drop x = toObject x %. "slice"

-- |
-- Returns the array @[ x[a],x[a+1] ... x[b] ]@, or equivalently
--
-- > x.slice(a, b)
--
slice :: JsArray -> Int -> Int -> IO JsArray
slice x a b = (toObject x %.. "slice") a b

--
-- $StringToArray
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
-- A string is a sequence of Unicode characters. Allthough it is normally used for text, it can be used to
-- store any unsigned 16-bit value using 'charAt' and 'fromCharCode'. Functions operating on 'JsString'
-- are generally an order of magniture faster than their 'String' equivalents. However 'JsString' is
-- strict and does not provide the full list interface.
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
fromCharCode = unsafeGlobalLookup ["String"] !%. "fromCharCode"

-- |
-- Returns the length of the given array, or equivalently
--
-- > x.length
--
stringLength :: JsString -> Int
stringLength x = toObject x !% "length"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > x.charAt(a)
--
charAt :: JsString -> Int -> JsString
charAt x = toObject x !%. "charAt"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > x.charCodeAt(a)
--
charCodeAt :: JsString -> Int -> Int
charCodeAt x = toObject x !%. "charCodeAt"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.indexOf.call(s, c)
--
indexOf :: JsString -> JsString -> Int
indexOf x = toObject x !%. "indexOf"

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > String.prototype.lastIndexOf.call(s, c)
--
lastIndexOf :: JsString -> JsString -> Int
lastIndexOf x = toObject x !%. "lastIndexOf"

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
        split' x = toObject x !%. "split"

-- |
-- Returns
--
-- > x.slice(a, b)
--
sliceString :: JsString -> Int -> Int -> JsString
sliceString x a b = (toObject x !%.. "slice") a b

-- |
-- Returns
--
-- > x.toLower()
--
toLower :: JsString -> JsString
toLower x = toObject x !%% "toLower"

-- |
-- Returns
--
-- > x.toUpper()
--
toUpper :: JsString -> JsString
toUpper x = toObject x !%% "toUpper"


-------------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------------

-- |
-- A JavaScript function, .
--
-- This type is disjoint from ordinary Haskell functions, which have a compiler-specific internal
-- representation. To convert between the two, use 'call' and 'lift'.
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
arity x = toObject x !% "length"

-- |
-- Existential wrapper for 'JsVal'.
data JsArg = forall a . JsVal a => JsArg a

new' :: JsVal a => JsFunction -> [JsArg] -> IO a
new' f []                           = new f
new' f [JsArg a]                    = new1 f a
new' f [JsArg a, JsArg b]           = new2 f a b
new' f [JsArg a, JsArg b, JsArg c]  = new3 f a b c
new' f _                            = error "new': Too many arguments"

call' :: JsVal a => JsFunction -> [JsArg] -> IO a
call' f []                          = call f
call' f [JsArg a]                   = call1 f a
call' f [JsArg a, JsArg b]          = call2 f a b
call' f [JsArg a, JsArg b, JsArg c] = call3 f a b c
call' f _                           = error "call': Too many arguments"


-------------------------------------------------------------------------------------

foreign import ccall "aPrimCall0"     call#   :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall1"     call1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall2"     call2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall3"     call3#  :: Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall4"     call4#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#

foreign import ccall "aPrimNew0"      new#    :: Any# -> IO Any#
foreign import ccall "aPrimNew1"      new1#   :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimNew2"      new2#   :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimNew3"      new3#   :: Any# -> Any# -> Any# -> Any# -> IO Any#

foreign import ccall "aPrimBind0"     bind#   :: Any# -> Any# -> Any#
foreign import ccall "aPrimBind1"     bind1#  :: Any# -> Any# -> Any# -> Any#

foreign import ccall "aPrimLift0"     lift#   :: Any# -> Any#
foreign import ccall "aPrimLift1"     lift1#  :: Any# -> Any#
foreign import ccall "aPrimLift2"     lift2#  :: Any# -> Any#

foreign import ccall "aPrimLiftPure0" liftp#  :: Any# -> Any#
foreign import ccall "aPrimLiftPure1" liftp1# :: Any# -> Any#
foreign import ccall "aPrimLiftPure2" liftp2# :: Any# -> Any#

--
-- $functionLaws
--
-- The following law hold for all functions:
--
-- > lift . call  = id
-- > call . lift  = id
-- > unsafeLift . unsafeCall = id
-- > unsafeCall . unsafeLift = id
--

call  :: JsVal a => JsFunction -> IO a
call1 :: (JsVal a, JsVal b) => JsFunction -> a -> IO b
call2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> IO c
call3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> a -> b -> c -> IO d
call4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> a -> b -> c -> d -> IO e

new  :: JsVal a => JsFunction -> IO a
new1 :: (JsVal a, JsVal b) => JsFunction -> a -> IO b
new2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> IO c
new3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> a -> b -> c -> IO d

lift  :: JsVal a => IO a -> JsFunction
lift1 :: (JsVal a, JsVal b) => (a -> IO b) -> JsFunction
lift2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> IO c) -> JsFunction

-- |
-- Partially apply the given function, or equivalently
--
-- > f.bind(null, a)
--
bind :: JsVal a => JsFunction -> a -> JsFunction



-- |
-- Apply the given function with the given @this@ value, or equivalently
--
-- > f.call(thisArg)
--
callWith :: JsVal a => JsFunction -> JsObject -> IO a
callWith1 :: (JsVal a, JsVal b) => JsFunction -> JsObject -> a -> IO b
callWith2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> JsObject -> a -> b -> IO c
callWith3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsFunction -> JsObject -> a -> b -> c -> IO d
callWith4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> JsObject -> a -> b -> c -> d -> IO e

-- |
-- Partially apply the given function with the given @this@ value, or equivalently
--
-- > f.bind(thisArg)
--
bindWith :: JsFunction -> JsObject -> JsFunction
bindWith1 :: JsVal a => JsFunction -> JsObject -> a -> JsFunction

call    = flip callWith  $ toObject ()
call1   = flip callWith1 $ toObject ()
call2   = flip callWith2 $ toObject ()
call3   = flip callWith3 $ toObject ()
call4   = flip callWith4 $ toObject ()
bind    = flip bindWith1 $ toObject ()

callWith  f t       = pure fromAny# <*> call# (getJsFunction f) (toAny# t)
callWith1 f t a     = pure fromAny# <*> call1# (getJsFunction f) (toAny# t) (toAny# a)
callWith2 f t a b   = pure fromAny# <*> call2# (getJsFunction f) (toAny# t) (toAny# a) (toAny# b)
callWith3 f t a b c = pure fromAny# <*> call3# (getJsFunction f) (toAny# t) (toAny# a) (toAny# b) (toAny# c)
callWith4 f t a b c d 
                    = pure fromAny# <*> call4# (getJsFunction f) (toAny# t) (toAny# a) (toAny# b) (toAny# c) (toAny# d)

new  f              = pure fromAny# <*> new# (getJsFunction f)
new1 f a            = pure fromAny# <*> new1# (getJsFunction f) (toAny# a)
new2 f a b          = pure fromAny# <*> new2# (getJsFunction f) (toAny# a) (toAny# b)
new3 f a b c        = pure fromAny# <*> new3# (getJsFunction f) (toAny# a) (toAny# b) (toAny# c)

bindWith  f t       = fromAny# $ bind# (getJsFunction f) (toAny# t)
bindWith1 f t a     = fromAny# $ bind1# (getJsFunction f) (toAny# t) (toAny# a)

lift        = JsFunction . lift#   . toAny# . toPtr#
lift1       = JsFunction . lift1#  . toAny# . toPtr#
lift2       = JsFunction . lift2#  . toAny# . toPtr#

unsafeLift  = JsFunction . liftp#  . toAny# . toPtr#
unsafeLift1 = JsFunction . liftp1# . toAny# . toPtr#
unsafeLift2 = JsFunction . liftp2# . toAny# . toPtr#



-------------------------------------------------------------------------------------

-- |
-- Invokeoke the method of the given name on the given object, or equivalently
--
-- > o.n()
--
invoke :: JsVal a => JsObject -> JsName -> IO a

-- |
-- Invokeoke the method of the given name on the given object, or equivalently
--
-- > o.n(a)
--
invoke1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> IO b

-- |
-- Invokeoke the method of the given name on the given object, or equivalently
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

invoke3 o n a b c = do
    f <- get o n
    callWith3 f o a b c

invoke4 o n a b c d = do
    f <- get o n
    callWith4 f o a b c d


-------------------------------------------------------------------------------------

unsafeCall :: JsVal a => JsFunction -> a
unsafeCall1 :: (JsVal a, JsVal b) => JsFunction -> a -> b
unsafeCall2 :: (JsVal a, JsVal b, JsVal c) => JsFunction -> a -> b -> c
unsafeLift :: JsVal a => a -> JsFunction
unsafeLift1 :: (JsVal a, JsVal b) => (a -> b) -> JsFunction
unsafeLift2 :: (JsVal a, JsVal b, JsVal c) => (a -> b -> c) -> JsFunction
unsafeInvoke :: JsVal a => JsObject -> JsName -> a
unsafeInvoke1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> b
unsafeInvoke2 :: (JsVal a, JsVal b, JsVal c) => JsObject -> JsName -> a -> b -> c

unsafeCall f          = unsafePerformIO $ call f
unsafeCall1 f a       = unsafePerformIO $ call1 f a
unsafeCall2 f a b     = unsafePerformIO $ call2 f a b

unsafeInvoke  o n     = unsafePerformIO $ invoke  o n
unsafeInvoke1 o n a   = unsafePerformIO $ invoke1 o n a
unsafeInvoke2 o n a b = unsafePerformIO $ invoke2 o n a b
unsafeInvoke3 o n a b c = unsafePerformIO $ invoke3 o n a b c 
unsafeInvoke4 o n a b c d = unsafePerformIO $ invoke4 o n a b c d

-------------------------------------------------------------------------------------


infixl 9 !%%
infixl 9 !%.
infixl 9 !%..
infixl 9 %%
infixl 9 %.
infixl 9 %..
infixl 9 %...
infixl 9 %....
infixl 9 %
infixl 9 %?

-- |
-- Infix version of 'invoke'.
--
(%%)   = invoke

-- |
-- Infix version of 'invoke1'.
--
(%.)  = invoke1

-- |
-- Infix version of 'invoke2'.
--
(%..) = invoke2
(%...) = invoke3
(%....) = invoke4

-- |
-- Infix version of 'unsafeInvoke'.
--
(!%%)   = unsafeInvoke

-- |
-- Infix version of 'unsafeInvoke1'.
--
(!%.)  = unsafeInvoke1

-- |
-- Infix version of 'unsafeInvoke2'.
--
(!%..) = unsafeInvoke2
(!%...) = unsafeInvoke3
(!%....) = unsafeInvoke4

-- |
-- Infix version of 'get'.
--
(%) = get

-- |
-- Infix version of 'unsafeGet'.
--
(!%) = unsafeGet

-- |
-- Infix version of 'hasProperty'.
--
(%?) = hasProperty


-------------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------------

--  /ECMA-262 15.x/
newtype JsDate = JsDate { getJsDate :: Any# }


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

foreign import ccall "aPrimWrite"     documentWrite#    :: String# -> IO ()
foreign import ccall "aPrimLog"       consoleLog#       :: String# -> IO ()
foreign import ccall "aPrimAlert"     alert#            :: String# -> IO ()

-- |
-- Displays a modal window with the given text.
--
alert :: JsString -> IO ()
alert str  = alert# (getJsString $ str)

-- |
-- Posts a line to the web console.
--
printLog :: JsString -> IO ()
printLog str = consoleLog# (getJsString $ str)

-- |
-- Appends the given content at the end of the `body` element.
--
printDoc :: JsString -> IO ()
printDoc str = documentWrite# (getJsString $ str)

-------------------------------------------------------------------------------------

foreign import ccall "aPrimEval"  eval#  :: String# -> IO Any#

-- |
-- Evaluates the given string as JavaScript.
--
-- /ECMA-262 15.1.2.1/
--
eval :: JsVal a => JsString -> IO a
eval = fmap fromAny# . eval# . getJsString

-------------------------------------------------------------------------------------

foreign import ccall "aPrimLog"   printRepr# :: Any# -> IO ()
foreign import ccall "aPrimDebug" debug#     :: IO ()

-- |
-- Prints the JavaScript representation of the given Haskell value.
--
-- The JavaScript representation of an arbitrary Haskell type (that is, one that is not an instance
-- of 'JsVal') is implementation-specific and should not be relied upon. However, it may be useful
-- to be able to inspect it in certain situations.
--
--
printRepr :: a -> IO ()
printRepr = printRepr# . toAny# . toPtr#
{-# NOINLINE printRepr #-}

-- |
-- Activates the JavaScript debugger, if there is one.
--
-- /ECMA-262 12.15/
--
debug :: IO ()
debug = debug#
{-# NOINLINE debug #-}

