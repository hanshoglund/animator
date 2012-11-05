
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.JQuery
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for JQuery 1.8.2.
--

-------------------------------------------------------------------------------------

module Web.JQuery (
        
        Query(..),
        query,
        fadeIn,
        fadeOut,
        fadeInSlow,
        fadeInDuring,
)
where

import Foreign.JavaScript

data Query
instance JsVal Query
instance JsRef Query

query :: JsString -> IO Query
query = call1 $ unsafeGlobalLookup ["jQuery"]

fadeIn :: Query -> IO ()
fadeIn x = toObject x %% "fadeIn"

fadeOut :: Query -> IO ()
fadeOut x = toObject x %% "fadeOut"

fadeInSlow :: Query -> IO ()
fadeInSlow x = toObject x %. "fadeIn" $ str "slow"

fadeInDuring :: Double -> Query -> IO ()
fadeInDuring n x = toObject x %. "fadeIn" $ n







with :: b -> a -> b
with x _ = x

fix :: a -> (a -> b) -> a
fix x _ = x
fix1 :: (a -> b) -> (a -> a) -> a -> b
fix1 x _ = x

int :: Int -> Int
int = id
str :: JsString -> JsString
str = id

