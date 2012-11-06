
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

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

        -- ** Queries
        Element(..),
        Query(..),
        query,
        isEmpty,
        isSingle,
        length,
        take,
        drop,
        slice,
        children,
        closest,
        filter,

        -- ** Effects
        hide,
        unhide,
        toggle,
        fadeIn,
        fadeInSlow,
        fadeInDuring,
        fadeOut,
        fadeOutSlow,
        fadeOutDuring,
        -- fadeTo,
        -- fadeToggle,

        slideUp,
        slideDown,
        slideToggle,
        
        -- stop,
        -- queue,
        -- animate,
        -- delay,
        -- clearQueue,
        -- dequeue,

)
where

import Prelude hiding (take, drop, filter, length)
import Foreign.JavaScript hiding (take, drop, slice, length)

-------------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------------

data Element
instance JsVal Element
instance JsRef Element

data Query
instance JsVal Query
instance JsRef Query
-- instance Semigroup Query

-- |
-- Perform a query.
query :: JsString -> IO Query
query = call1 $ unsafeGlobalLookup ["jQuery"]

isEmpty :: Query -> Bool
isEmpty = undefined

isSingle :: Query -> Bool
isSingle = undefined

length :: Query -> Int
length = undefined

take :: Int -> Query -> Query
take = undefined
drop :: Int -> Query -> Query
drop = undefined
slice :: Int -> Int -> Query -> Query
slice = undefined

children :: JsString -> Query -> Query
children = undefined
closest :: JsString -> Query -> Query
closest = undefined
filter :: (Element -> Bool) -> Query -> Query
filter = undefined
-- contents :: Query -> [?]
-- children :: Query -> [?]

-- TODO


-------------------------------------------------------------------------------------
-- Effects
-------------------------------------------------------------------------------------

hide :: Query -> IO ()
hide x = toObject x %% "hide"

unhide :: Query -> IO ()
unhide x = toObject x %% "show"

toggle :: Query -> IO ()
toggle x = toObject x %% "toggle"

fadeIn :: Query -> IO ()
fadeIn x = toObject x %% "fadeIn"

fadeInFast :: Query -> IO ()
fadeInFast x = toObject x %. "fadeIn" $ str "fast"

fadeInSlow :: Query -> IO ()
fadeInSlow x = toObject x %. "fadeIn" $ str "slow"

fadeInDuring :: Double -> Query -> IO ()
fadeInDuring n x = toObject x %. "fadeIn" $ n

fadeOut :: Query -> IO ()
fadeOut x = toObject x %% "fadeOut"

fadeOutFast :: Query -> IO ()
fadeOutFast x = toObject x %. "fadeOut" $ str "fast"

fadeOutSlow :: Query -> IO ()
fadeOutSlow x = toObject x %. "fadeOut" $ str "slow"

fadeOutDuring :: Double -> Query -> IO ()
fadeOutDuring n x = toObject x %. "fadeOut" $ n

slideUp :: Query -> IO ()
slideUp x = toObject x %% "slideUp"

slideDown :: Query -> IO ()
slideDown x = toObject x %% "slideDown"

slideToggle :: Query -> IO ()
slideToggle x = toObject x %% "slideToggle"


-------------------------------------------------------------------------------------
-- Manipulation
-------------------------------------------------------------------------------------






-------------------------------------------------------------------------------------

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

