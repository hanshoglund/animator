
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
            fill g $ blue `withOpacity` 0.2
            scale g 0.9
            rect g (-1/2) (-1/2) 1 1


        handler2b g = do            
            stroke g transparent
            fill g $ red `withOpacity` 0.2                        
            scale g 0.8
            rect g (-1/2) (-1/2) 1 1
        
        handler3 g = do
            stroke g transparent
            fill g $ red `withOpacity` 0.2
            translate g 0.2 (0.3)
            scaleX g 0.3
            scaleY g 0.1
            circle g
            
        handler p = do
            size p 600 600
            withGraphics p 600 600 (renderDrawInstr 600 handler3)
            withGraphics p 600 600 (renderDrawInstr 600 handler2)
            withGraphics p 600 600 (renderDrawInstr 600 handler2b)
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


