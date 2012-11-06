
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
        Size,
        Buffer,
        slice,
        slice',

        -- ** Views
        HasView(..),
        BufferView,
        buffer,
        byteOffset,
        byteSize,
        size,
        length,
        get,
        set,
        subView
)
where

import Prelude hiding (take, drop, filter, length)
import Data.Int
import Data.Word
import Foreign.JavaScript hiding (set, get, take, drop, slice, length)

-- |
-- Offset into a buffer in bytes.
type Offset = Int

-- |
-- Size of a buffer. Either refers to a raw size in bytes or an element count.
type Size = Int

data Buffer

slice :: Offset -> Buffer -> IO Buffer
slice = undefined
slice' :: Offset -> Size -> Buffer -> IO Buffer
slice' = undefined



class HasView a where
    view :: Buffer -> Offset -> Size -> BufferView a
    view = undefined
instance HasView Int8
instance HasView Int16
instance HasView Int32
instance HasView Word8
instance HasView Word16
instance HasView Word32
instance HasView Float
instance HasView Double

data BufferView a
buffer     :: HasView a => BufferView a -> Buffer
buffer = undefined
byteOffset :: HasView a => BufferView a -> Offset
byteOffset = undefined
byteSize :: HasView a => BufferView a -> Size
byteSize = undefined
size       :: BufferView a -> Size
size = undefined
length     :: BufferView a -> Size
length = undefined

get :: BufferView a -> IO a
get = undefined
set :: a -> BufferView a -> IO ()
set = undefined

subView :: Offset -> Size -> BufferView a -> BufferView a
subView = undefined


