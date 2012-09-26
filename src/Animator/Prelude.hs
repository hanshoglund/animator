
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Animator.Prelude
( 
-- * Basic types
-- ** Numeric values
Bool,
Int,
Char,
Double,

-- ** Compound types
String,

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
Bounded,
Enum,
Eq,
Ord,
Show,
Num,
Real,
Fractional,
Floating,

-- * Host types
JsString,
JsObject,
JsArray,
)
where
    
import Animator.Internal.Prim
import Data.Maybe
import Data.Either