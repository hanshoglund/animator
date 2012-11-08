
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Control.Applicative
import Foreign.JavaScript
import Web.Graphics.Processing

mouseX = fmap fst mouseS
mouseY = fmap snd mouseS
always  = pure
fill    = fillA
scale   = scaleA
circle  = circleA

a x c = fill (always $ c `withOpacity` 0.3) 
    . scale (sin $ timeS / 600 * x * tau) 
    $ circle

b x c = fill (always $ c `withOpacity` 0.3) 
    . scale (negate $ mouseX / 600 * x) 
    $ circle

foobar = a 11 red <> b 1 blue

main = runAnimation foobar "main-canvas"
