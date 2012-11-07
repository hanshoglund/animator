
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
        Size,

        -- ** Buffers
        Buffer,
        newBuffer,
        slice,

        -- ** Typed arrays/Views
        HasView(..),
        -- Clamped(..),
        BufferView,
        newBufferView,

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
import System.IO.Unsafe

import Foreign.JavaScript hiding (set, get, take, drop, slice, length)

import qualified Prelude as P
import qualified Foreign.JavaScript as JS

-------------------------------------------------------------------------------------
-- Buffers
-------------------------------------------------------------------------------------

type Size = Int

data Buffer
instance JsVal Buffer
instance JsRef Buffer

newBuffer :: Size -> IO Buffer
newBuffer n = unsafeGlobalLookup ["ArrayBuffer"] `new1` n

newBufferView :: HasView a => Size -> IO (BufferView a)
newBufferView n = do
    b <- newBuffer n
    return $ getView b 0 n

-- |
-- Slice buffer bitwise.
slice :: Buffer -> Size -> Size -> IO Buffer
slice x a b = (toObject x %.. "slice") a b


-------------------------------------------------------------------------------------
-- Typed arrays/views
-------------------------------------------------------------------------------------

class HasView a where
    getView :: Buffer -> Size -> Size -> BufferView a

instance HasView Int8 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Int8Array"] `new3`) x a b
instance HasView Int16 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Int15Array"] `new3`) x a b
instance HasView Int32 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Int32Array"] `new3`) x a b
instance HasView Word8 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Uint8Array"] `new3`) x a b
instance HasView Word16 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Uint16Array"] `new3`) x a b
instance HasView Word32 where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Uint32Array"] `new3`) x a b
instance HasView Float where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Float32Array"] `new3`) x a b
instance HasView Double where
    getView x a b = unsafePerformIO $ (unsafeGlobalLookup ["Float64Array"] `new3`) x a b

-- newtype Clamped a = Clamped { getClamped :: a }
-- instance HasView (Clamped Word8)

data BufferView a
instance HasView a => JsVal (BufferView a)
instance HasView a => JsRef (BufferView a)

buffer :: HasView a => BufferView a -> Buffer
buffer x = toObject x !% "buffer"

-- | Byte offset
offset :: HasView a => BufferView a -> Size
offset x = toObject x !% "byteOffset"

-- | Byte length
size :: HasView a => BufferView a -> Size
size x = toObject x !% "byteLength"

-- | Number of elements
length :: HasView a => BufferView a -> Size
length x = toObject x !% "lenght"

get :: HasView a => BufferView a -> Size -> IO a
get x j = error "Not implemented"

set :: a -> HasView a => BufferView a -> Size -> a -> IO ()
set x j = error "Not implemented"

setRange :: a -> HasView a => BufferView a -> Size -> BufferView a -> IO ()
setRange x j y = error "Not implemented"

getRange :: HasView a => Size -> Int -> BufferView a -> BufferView a
getRange x i j = error "Not implemented"


