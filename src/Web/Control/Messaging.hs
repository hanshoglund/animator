
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Control.Messaging
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Web Messaging.
--
-- * <http://caniuse.com/#search=messaging>
--
-- * <http://www.whatwg.org/specs/web-apps/current-work/multipage/comms.html>
--

-------------------------------------------------------------------------------------

module Web.Control.Messaging -- (
-- )
where

import Prelude -- hiding (take, drop, filter, length)
import Foreign.JavaScript -- hiding (take, drop, slice, length)


