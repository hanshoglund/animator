
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Document
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Document Object Model (DOM).
--
-- * <http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407>
--

-------------------------------------------------------------------------------------

module Web.Document -- (
-- )
where

import Prelude hiding (lookup)
import Foreign.JavaScript -- hiding (take, drop, slice, length)

data Document
instance JsVal Document
instance JsRef Document

document :: IO Document
document = do
    g <- global
    lookup g ["document"]

data Element
instance JsVal Element
instance JsRef Element

getElementById :: Document -> JsString -> IO Element
getElementById x = toObject x %. "getElementById"

requestFullScreen :: Element -> IO ()
requestFullScreen x = toObject x % "requestFullScreen"