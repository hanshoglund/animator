
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Graphics.Processing
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Processing.js 1.4.1.
--

-------------------------------------------------------------------------------------

module Web.Graphics.Processing -- (
--  )
where

import Prelude hiding (lookup)
import Web.Document

import Foreign.JavaScript -- hiding (take, drop, slice, length)

data Processing
instance JsVal Processing
instance JsRef Processing

runProcessing :: (Processing -> IO ()) -> JsString -> IO ()
runProcessing f n = do
    d <- document
    e <- getElementById d n
    p <- mkProcessing e (lift1 f) :: IO Processing
    return ()
    where
        mkProcessing = new2 $ unsafeGlobalLookup ["Processing"]