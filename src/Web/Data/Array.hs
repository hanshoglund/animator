
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-- {-# LANGUAGE FlexibleInstances #-}

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

        -- ** Size types
        Offset,
        Size,
        Index,
        Length,

        -- ** Buffers
        Buffer,
        slice,

        -- ** Typed arrays/Views
        HasView(..),
        -- Clamped(..),
        BufferView,

        -- *** Properties
        buffer,
        offset,
        size,
        length,
        
        -- *** Single element read/write
        get,
        set,

        -- *** Contigous read/write
        setRange,
        getRange,
)
where

import Prelude hiding (take, drop, filter, length)
import Data.Int
import Data.Word

import Foreign.JavaScript hiding (set, get, take, drop, slice, length)

import qualified Prelude as P
import qualified Foreign.JavaScript as JS

-------------------------------------------------------------------------------------
-- Buffers
-------------------------------------------------------------------------------------

-- |
-- Offset in bytes.
type Offset = Int

-- |
-- Raw size in bytes.
type Size = Int

-- |
-- Index of element.
type Index = Int

-- |
-- Number of elements.
type Length = Int

data Buffer
instance JsVal Buffer
instance JsRef Buffer

create :: Size -> IO Buffer
create = undefined
-- create s = unsafeLookupGlobal ["ArrayBuffer"] `new` [s]

slice :: Offset -> Offset -> Buffer -> IO Buffer
slice = undefined



-------------------------------------------------------------------------------------
-- Typed arrays/views
-------------------------------------------------------------------------------------

class HasView a where
    getView :: Buffer -> Offset -> Size -> BufferView a
    getView = undefined
instance HasView Int8
instance HasView Int16
instance HasView Int32
instance HasView Word8
instance HasView Word16
instance HasView Word32
instance HasView Float
instance HasView Double

-- newtype Clamped a = Clamped { getClamped :: a }
-- instance HasView (Clamped Word8)

data BufferView a
instance HasView a => JsVal (BufferView a)
instance HasView a => JsRef (BufferView a)

buffer :: HasView a => BufferView a -> Buffer
buffer = undefined

offset :: HasView a => BufferView a -> Offset
offset = undefined

-- | Number of bytes
size :: HasView a => BufferView a -> Size
size = undefined

-- | Number of elements
length :: HasView a => BufferView a -> Size
length x = toObject x !% "lenght"

get :: HasView a => BufferView a -> Index -> IO a
get x j = undefined

set :: a -> HasView a => BufferView a -> Index -> a -> IO ()
set x j = undefined

setRange :: a -> HasView a => BufferView a -> Index -> BufferView a -> IO ()
setRange x j y = undefined

getRange :: HasView a => Index -> Int -> BufferView a -> BufferView a
getRange x i j = undefined


