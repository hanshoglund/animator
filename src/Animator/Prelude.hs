
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Module      : Animator.Prelude
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
-- 
-- This module is impicitly imported into every Animator program.           
--
-- Haskell programmers can use this with the @NoImplicitPrelude@ extention, but it is
-- not mandatory, the standard Haskell prelude works just as fine.
--
-------------------------------------------------------------------------------------

module Animator.Prelude ( 
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
        MonadFix(..),

        -- * Host language
  ) where
    
import Data.Eq
import Data.Ord
import Data.Semigroup
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Fix

import Data.Foldable
import Data.Traversable

import Data.Maybe
import Data.Either
import Data.Word
import Data.String

import Foreign.JavaScript

-- type Rat = Rational Int

data TotalOrdering = GT | LT
class Eq a => TotalOrd a where
    compareTotal :: a -> a -> TotalOrdering
    
comparingTotal :: TotalOrd a => (b -> a) -> b -> b -> TotalOrdering
comparingTotal = undefined