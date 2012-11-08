
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Control.Applicative
import Foreign.JavaScript
import Web.Graphics.Processing

mouseX  = fmap fst mouseS
mouseY  = fmap snd mouseS
always  = pure
fill    = fillA
scale   = scaleA
circle  = circleA

oscCircle x c = 
    style . shape $ circle
        where
            shape = scale $ (sin $ timeS / 600 * x * tau) * (mouseY/600)
            style = fill $ always $ c `withOpacity` 0.3

mouseCircle x c = style . shape $ circle
    where
        style = fill (always $ c `withOpacity` 0.3) 
        shape = scale (negate $ mouseX / 600 * x) 

test = oscCircle 11 red <> mouseCircle 1 blue




main = runAnimation test "main-canvas"
