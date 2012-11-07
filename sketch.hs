
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Data.Semigroup
import Control.Applicative
import Foreign.JavaScript
import Web.Graphics.Processing

main = do
    runProcessing handler "main-canvas"
    where                                  
        -- foo g = do            
        --      stroke g transparent
        --      fill g $ blue `withOpacity` 0.2
        --      scale g 0.9
        --      rect g (-1/2) (-1/2) 1 1
        -- 
        -- 
        --  bar g = do            
        --      stroke g transparent
        --      fill g $ red `withOpacity` 0.2                        
        --      scale g 0.8
        --      rect g (-1/2) (-1/2) 1 1
        --  
        --  baz g = do
        --      stroke g transparent
        --      fill g $ red `withOpacity` 0.2
        --      translate g 0.2 (0.3)
        --      scaleX g 0.3
        --      scaleY g 0.1
        --      circle g
             
        handler p = do          
            let a = scaleA (fmap (/600) timeS) circleA
            let b = scaleA (fmap ((/600) . fst) mouseS) circleA


            setDraw p (flip renderAnimation $ a <> b)
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


