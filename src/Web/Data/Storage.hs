
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Data.Storage
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Web Storage.
--
-- * <http://caniuse.com/#search=storage>
--
-- * <http://www.w3.org/TR/webstorage>
--

-------------------------------------------------------------------------------------

module Web.Data.Storage -- (
-- )
where

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)


