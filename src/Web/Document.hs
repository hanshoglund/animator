
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

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)


