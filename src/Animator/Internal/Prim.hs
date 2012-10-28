
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances, ForeignFunctionInterface, OverloadedStrings, CPP,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

-- |
-- The 'Animator.Primitive' modules provide a dynamic interface to the JavaScript host environment.
-- It provides access to JavaScript types including strings, arrays, objects and functions.
--
-- Objects can be inspected, created and updated at runtime. JavaScript functions can be called,
-- partially applied or passed to other JavaScripf functions. Haskell functions can be converted
-- to JavaScript functions for use as callbacks in the host environment.
--

-------------------------------------------------------------------------------------

-- TODO Remove TypeSynonymInstances, FlexibleInstances?
--      Required for String instance of JsProp, can we rethink this?

module Animator.Internal.Prim (
        JsVal(..),
        eval,

        JsRef(..),
        JsName,
        JsProp(..),

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
        call,
        call1,
        call2,
        call3,
        call4,
        bind,
        bind1,
        bind2,
        bind3,
        bind4,
        invoke,
        invoke1,
        invoke2,
        invoke3,
        invoke4,

        -- ** With object
        callWith,
        callWith1,
        callWith2,
        callWith3,
        callWith4,
        bindWith,
        bindWith1,
        bindWith2,
        bindWith3,
        bindWith4,

        -- ** Infix operators
        (%%),
        (%%!),
        (%%!!),
        -- apply,
        -- new,

        -- ** JSON
        JSON,
        parse,
        stringify,

        -- ** Utility
        alert,
        windowConsoleLog,
        windowDocumentWrite,        
        printRepr,
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
foreign import ccall "aPrimEval"      eval#             :: String# -> IO Any#


-- |
-- Class of JavaScript types. 
class JsVal a where
    
    -- | Returns a string describing a type of the given value, or equivalently
    --
    -- > typeof x
    typeOf :: JsVal a => a -> String

-- |
-- Evaluate the given string as JavaScript, or equivalently
--
-- > eval(s) 
eval :: JsVal a => JsString -> IO a
eval = unsafeCoerce . eval# . getJsString

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
    typeOf _ = "object"
instance JsVal JsArray where
    typeOf _ = "object"
instance JsVal JsFunction where
    typeOf _ = "function"
    
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
    -- | @get n o@ fetches the value named @n@ from object @o@, or equivalently
    --
    -- > o.n
    get :: JsObject -> JsName -> IO a

    -- | @set n o x@ assigns the property @n@ to @x@ in object @o@, or equivalently
    --
    -- > o.n = x
    set :: JsObject -> JsName -> a -> IO ()

    -- | @update n o f@ updates the value named @n@ in object @o@ by applying the function f,
    --   or equivalently
    --
    -- > x.n = f(x.n)
    update :: JsObject -> JsName -> (a -> a) -> IO ()
    update n o f = get n o >>= set n o . f



-------------------------------------------------------------------------------------
-- Objects
-------------------------------------------------------------------------------------

foreign import ccall "aPrimObj"       object#           :: IO Any#
foreign import ccall "aPrimGlobal"    global#           :: IO Any#
foreign import ccall "aPrimNull"      null#             :: Any#

-- |
-- A JavaScript object.
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
null = JsObject $ null# 

-- |
-- Returns the JavaScript global object, or equivalently
--
-- > window
global :: IO JsObject
global = global# >>= (return . JsObject)

-- |
-- Returns true if the specified object is of the specified object type, or equivalently
--
-- > x instanceof y
isInstanceOf :: JsObject -> JsObject -> Bool
isInstanceOf = error "Not implemented"

-- |
-- Returns
--
-- > x.isPrototypeOf(y)
isPrototypeOf :: JsObject -> JsObject -> Bool
isPrototypeOf = error "Not implemented"

-- |
-- Returns
--
-- > x.constructor(y)
constructor :: JsObject -> JsFunction
constructor = error "Not implemented"

-- |
-- Returns
--
-- > x.hasOwnProperty(y)
hasOwnProperty :: JsName -> JsObject -> IO Bool
hasOwnProperty = error "Not implemented"

-- |
-- Returns
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

-- |
-- Returns
--
-- > x.toString(y)
toString :: JsObject -> JsString
toString = error "Not implemented"

-- |
-- Returns
--
-- > x.toLocaleString(y)
toLocaleString :: JsObject -> JsString
toLocaleString = error "Not implemented"

-- |
-- Returns
--
-- > x.valueOf(y)
valueOf :: JsVal a => JsObject -> a
valueOf = error "Not implemented"


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

foreign import ccall "aPrimArrConcat" concatArray#      :: Any# -> Any# -> Any#

-- |
-- A JavaScript array.
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
pop :: JsVal a => JsArray -> IO a
pop = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.push.call(x, v)
push :: JsVal a => a -> JsArray -> IO JsArray
push = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.shift.call(x)
shift :: JsVal a => JsArray -> IO a
shift = error "Not implemented"

-- |
-- Returns a string describing a type of the given object, or equivalently
--
-- > Array.prototype.unshift.call(x, v)
unshift :: JsVal a => a -> JsArray -> IO JsArray
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
-- Strings
-------------------------------------------------------------------------------------

-- |
-- A JavaScript string.
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
-- A JavaScript function.
newtype JsFunction = JsFunction { getJsFunction :: Any# }

arity :: JsFunction -> Int
arity = error "Not implemented"

foreign import ccall "aPrimCall0" call#  :: Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall1" call1#  :: Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall2" call2#  :: Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall3" call3#  :: Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall4" call4#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#
foreign import ccall "aPrimCall5" call5#  :: Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> Any# -> IO Any#

call  f = callWith  f null
call1 f = callWith1 f null
call2 f = callWith2 f null
call3 f = callWith3 f null
call4 f = callWith4 f null

callWith :: JsVal b => JsFunction -> JsObject -> IO b
callWith f t = do
    r <- call# (getJsFunction f) (p t)
    return $ q r
    where 
        (p,q) = callPrePost

callWith1 :: (JsVal b, JsVal c) => JsFunction -> JsObject -> b -> IO c
callWith1 f t a = do
    r <- call1# (getJsFunction f) (p t) (p a)
    return $ q r
    where 
        (p,q) = callPrePost

callWith2 :: (JsVal b, JsVal c, JsVal d) => JsFunction -> JsObject -> b -> c -> IO d
callWith2 f t a b = do
    r <- call2# (getJsFunction f) (p t) (p a) (p b)
    return $ q r
    where 
        (p,q) = callPrePost

callWith3 :: (JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> JsObject -> b -> c -> d -> IO e
callWith3 f t a b c = do
    r <- call3# (getJsFunction f) (p t) (p a) (p b) (p c)
    return $ q r
    where 
        (p,q) = callPrePost

callWith4 :: (JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsFunction -> JsObject -> b -> c -> d -> e -> IO f
callWith4 f t a b c d = do
    r <- call4# (getJsFunction f) (p t) (p a) (p b) (p c) (p d)
    return $ q r
    where 
        (p,q) = callPrePost

callWith5 :: (JsVal b, JsVal c, JsVal d, JsVal e, JsVal f, JsVal g) => JsFunction -> JsObject -> b -> c -> d -> e -> f -> IO g
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

bind  f = bindWith  f null
bind1 f = bindWith1 f null
bind2 f = bindWith2 f null
bind3 f = bindWith3 f null
bind4 f = bindWith4 f null


bindWith :: JsFunction -> JsObject -> IO JsFunction
bindWith f t = do
    r <- bind# (getJsFunction f) (p t)
    return $ q r
    where 
        (p,q) = bindPrePost

bindWith1 :: (JsVal b) => JsFunction -> JsObject -> b -> IO JsFunction
bindWith1 f t a = do
    r <- bind1# (getJsFunction f) (p t) (p a)
    return $ q r
    where 
        (p,q) = bindPrePost

bindWith2 :: (JsVal b, JsVal c) => JsFunction -> JsObject -> b -> c -> IO JsFunction
bindWith2 f t a b = do
    r <- bind2# (getJsFunction f) (p t) (p a) (p b)
    return $ q r
    where 
        (p,q) = bindPrePost

bindWith3 :: (JsVal b, JsVal c, JsVal d) => JsFunction -> JsObject -> b -> c -> d -> IO JsFunction
bindWith3 f t a b c = do
    r <- bind3# (getJsFunction f) (p t) (p a) (p b) (p c)
    return $ q r
    where 
        (p,q) = bindPrePost

bindWith4 :: (JsVal b, JsVal c, JsVal d, JsVal e) => JsFunction -> JsObject -> b -> c -> d -> e -> IO JsFunction
bindWith4 f t a b c d = do
    r <- bind4# (getJsFunction f) (p t) (p a) (p b) (p c) (p d)
    return $ q r
    where 
        (p,q) = bindPrePost

bindWith5 :: (JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsFunction -> JsObject -> b -> c -> d -> e -> f -> IO JsFunction
bindWith5 f t a b c d e = do
    r <- bind5# (getJsFunction f) (p t) (p a) (p b) (p c) (p d) (p e)
    return $ q r
    where 
        (p,q) = bindPrePost

callPrePost = (unsafeCoerce, unsafeCoerce)
bindPrePost = (unsafeCoerce, unsafeCoerce)


infixl 9 %%
infixl 9 %%!
infixl 9 %%!!
(%%)   = invoke
(%%!)  = invoke1
(%%!!) = invoke2

invoke :: JsVal a => JsObject -> JsName -> IO a
invoke o n = do
    f <- get o n
    callWith f o

invoke1 :: (JsVal a, JsVal b) => JsObject -> JsName -> a -> IO b
invoke1 o n a = do
    f <- get o n
    callWith1 f o a

invoke2 :: (JsVal a, JsVal b, JsVal c) => JsObject -> JsName -> a -> b -> IO c
invoke2 o n a b = do
    f <- get o n
    callWith2 f o a b

invoke3 :: (JsVal a, JsVal b, JsVal c, JsVal d) => JsObject -> JsName -> a -> b -> c -> IO d
invoke3 o n a b c = do
    f <- get o n
    callWith3 f o a b c

invoke4 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e) => JsObject -> JsName -> a -> b -> c -> d -> IO e
invoke4 o n a b c d = do
    f <- get o n
    callWith4 f o a b c d

invoke5 :: (JsVal a, JsVal b, JsVal c, JsVal d, JsVal e, JsVal f) => JsObject -> JsName -> a -> b -> c -> d -> e -> IO f
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

foreign import ccall "aPrimLog"       printRepr#        :: Any#    -> IO ()
foreign import ccall "aPrimWrite"     documentWrite#    :: String# -> IO ()
foreign import ccall "aPrimLog"       consoleLog#       :: String# -> IO ()
foreign import ccall "aPrimAlert"     alert#            :: String# -> IO ()

-- |
-- Displays a modal window with the given text.
printRepr :: a -> IO ()
printRepr = printRepr# . unsafeCoerce . toPtr#
{-# NOINLINE printRepr #-}

-- |
-- Displays a modal window with the given text.
alert :: String -> IO ()
alert str  = alert# (toJsString# $ str)

-- |
-- Posts a line to the web console.
windowConsoleLog :: String -> IO ()
windowConsoleLog str = consoleLog# (toJsString# $ str)

-- |
-- Appends the given content at the end of the `body` element.
windowDocumentWrite :: String -> IO ()
windowDocumentWrite str = documentWrite# (toJsString# $ str)


