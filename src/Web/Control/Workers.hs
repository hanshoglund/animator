
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Control.Workers
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Web Workers.
--
-- * <http://caniuse.com/#search=workers>
--
-- * <http://www.whatwg.org/specs/web-workers/current-work>
--

-------------------------------------------------------------------------------------

module Web.Control.Workers -- (
-- )
where

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)


