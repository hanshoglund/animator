
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.System.File
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for System.File.
--
-- * <http://caniuse.com/#search=file%20api>
--
-- * <http://www.w3.org/TR/FileAPI/>
--

-------------------------------------------------------------------------------------

module Web.System.File (

        -- ** Blobs
        HasBlob(..),

        Blob,
        blobSize,
        blobType,
        sliceBlob,
        close,
        
        -- ** Files
        File,
        name,
        lastModified,
        
        -- ** File readers
        FileReader,
        UriScheme,
        
        -- ** Util
        toNativeLineEndings
)
where   
    
import Web.Data.Array

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)

class HasBlob a where
    getBlob :: a -> Blob
instance HasBlob File where
    getBlob = undefined

-- |
-- Immutable binary data.
data Blob
instance JsVal Blob
instance JsRef Blob
blobSize :: Blob -> Size
blobSize = undefined
blobType :: Blob -> JsString
blobType = undefined
sliceBlob :: Int -> Int -> Blob -> Blob
sliceBlob = undefined
close :: Blob -> IO ()
close = undefined

-- |
-- A file.
data File
instance JsVal File
instance JsRef File
name :: File -> JsString
name = undefined
lastModified :: File -> JsDate
lastModified = undefined

data FileReader
instance JsVal FileReader
instance JsRef FileReader

data UriScheme

toNativeLineEndings :: JsString -> JsString
toNativeLineEndings = undefined
