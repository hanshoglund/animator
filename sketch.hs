
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Control.Applicative
import Foreign.JavaScript
import Web.Graphics.Processing

a = fillA (pure $ red `withOpacity` 0.2) 
    . scaleA (fmap sin . fmap (*tau) . fmap (*13) 
    . fmap (/600) $ timeS) 
    $ circleA

b = fillA (pure $ blue `withOpacity` 0.2) 
    . scaleA (fmap ((/600) . fst) mouseS) 
    $ circleA

c = b <> a


main = runAnimation c "main-canvas"
