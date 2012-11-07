
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Foreign.JavaScript
import Web.Graphics.Processing

main = do
    runProcessing handler "main-canvas"
    where                                  
        handler2 g = do
            stroke g transparent
            translate g 300 300
            fill g $ blue `withOpacity` 0.5
            rect g 0 0 100 100
        handler3 g = do
            stroke g transparent
            translate g 300 300
            fill g $ red `withOpacity` 0.5
            scale g 200
            ellipse g 0 0 1 1
            
        handler p = do
            size p 600 600
            
            withGraphics p 600 600 handler2
            withGraphics p 600 600 handler3
            return ()






















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


