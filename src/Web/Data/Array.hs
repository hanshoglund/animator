
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Data.Array
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Typed Arrays.
--
-- * <http://caniuse.com/#feat=typedarrays>
--
-- * <http://www.khronos.org/registry/typedarray/specs/latest>
--

-------------------------------------------------------------------------------------

module Web.Data.Array (

        -- ** Buffers
        Offset,
        Length,
        Buffer,
        slice,
        slice',

        -- ** Views
        HasView(..),
        Array,
        buffer,
        byteOffset,
        byteLength,
        size,
        length,
        get,
        set,
        subArray
)
where

import Prelude hiding (take, drop, filter, length)
import Data.Int
import Data.Word
import Foreign.JavaScript hiding (set, get, take, drop, slice, length)

type Offset = Int
type Length = Int

data Buffer

slice :: Offset -> Buffer -> IO Buffer
slice = undefined
slice' :: Offset -> Length -> Buffer -> IO Buffer
slice' = undefined



class HasView a where
    view :: Buffer -> Offset -> Length -> Array a
    view = undefined
instance HasView Int8
instance HasView Int16
instance HasView Int32
instance HasView Word8
instance HasView Word16
instance HasView Word32
instance HasView Float
instance HasView Double

data Array a
buffer     :: HasView a => Array a -> Buffer
buffer = undefined
byteOffset :: HasView a => Array a -> Offset
byteOffset = undefined
byteLength :: HasView a => Array a -> Length
byteLength = undefined
size       :: Array a -> Length
size = undefined
length     :: Array a -> Length
length = undefined

get :: Array a -> IO a
get = undefined
set :: a -> Array a -> IO ()
set = undefined

subArray :: Offset -> Length -> Array a -> Array a
subArray = undefined


