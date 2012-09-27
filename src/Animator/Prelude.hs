
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Animator.Prelude
( 
-- * Basic types
-- ** Numeric values
Bool,
Char,
Int,
Word,
Double,

-- ** Compound types
IsString(..),
String,    
lines,
unlines,
unwords,

-- ** Alternatives
Maybe,
Either,
maybe,
isJust,
isNothing,
listToMaybe,
maybe,
maybeToList,

either,
lefts,
rights,
partitionEithers,


-- * Basic classes
Eq(..),

Ord(..),
Ordering,
comparing,

TotalOrd(..),
TotalOrdering,
comparingTotal,

Bounded(..),
Enum(..),
Show(..),
Num(..),
Real(..),
Fractional(..),
Floating(..),

Semigroup(..),
Monoid(..),
Functor(..),
Applicative(..),
Monad(..),

-- * Host language
JsString,
JsObject,
JsArray,

-- *** Objects
new,
JsProperty(..),
-- **** Concrete version
getStr,
setStr,
getInt,
setInt,

-- ** Basic I/O
consoleLog,
documentWrite,
alert,
)
where
    
import Animator.Internal.Prim
import Data.Maybe
import Data.Either
import Data.Word
import Data.String

import Data.Ord
import Data.Semigroup
import Data.Monoid
import Control.Applicative

data TotalOrdering = GT | LT
class Eq a => TotalOrd a where
    compareTotal :: a -> a -> TotalOrdering
    
comparingTotal :: TotalOrd a => (b -> a) -> b -> b -> TotalOrdering
comparingTotal = undefined