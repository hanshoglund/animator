
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
        handler p = do
            printRepr $! p
            size p 400 400
            background p red

            fill p blue
            ellipse p 100 0 200 200

            fill p green
            rect p 30 110 100 100

            -- println p "This is Processing!"
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


