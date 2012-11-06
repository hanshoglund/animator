
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

module Web.System.File -- (
-- )
where   
    
import Web.Data.Array

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)

-- |
-- Immutable binary data.
data Blob
sizeBlob :: Blob -> Size
sizeBlob = undefined
sliceBlob :: Int -> Int -> Blob -> Blob
sliceBlob = undefined
close :: Blob -> IO ()
close = undefined

-- |
-- A file.
data File



data FileReader
type FileList = [File]
data UriScheme

toNativeLineEndings :: JsString -> JsString
toNativeLineEndings = undefined
