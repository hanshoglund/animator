
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Control.Applicative
import Foreign.JavaScript
import Web.Graphics.Processing

a x c = fillA (pure $ c `withOpacity` 0.2) 
    . scaleA (fmap sin . fmap (*tau) . fmap (*x) 
    . fmap (/600) $ timeS) 
    $ circleA

b = fillA (pure $ blue `withOpacity` 0.2) 
    . scaleA (fmap ((* (-1)) . (/600) . fst) mouseS) 
    $ circleA

foobar = a 11 red <> a 12 green

main = runAnimation foobar "main-canvas"
