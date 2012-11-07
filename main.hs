
{-# LANGUAGE
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    MultiParamTypeClasses, FlexibleInstances,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Foreign.JavaScript
import Web.JQuery

import Haste.Showable(show_)
import Data.Foldable
import Data.Word

main = do
    testNew
    -- testUndefined
    -- testFib
    -- testReadShow
    -- testBool
    -- testString
    -- testPrim
    -- testLift
    -- testLookup
    -- testPropertyLookup
    testJQuery

{-# NOINLINE testUndefined #-}
{-# NOINLINE testFib #-}
{-# NOINLINE testReadShow #-}
{-# NOINLINE testBool #-}
{-# NOINLINE testString #-}
{-# NOINLINE testPrim #-}
{-# NOINLINE testJQuery #-}
{-# NOINLINE testLift #-}
{-# NOINLINE testLookup #-}




testNew = do
    date1 <- mkDate []
    date2 <- mkDate [JsArg ("Dec 26 1987"::JsString)]
    printRepr $! date1
    printRepr $! date2
    where
        mkDate = new' $ unsafeGlobalLookup ["Date"]
    
testReadShow = do
    printLog $ toJsString $ show_ (11232.12328::Double)
    printLog $ toJsString $ show  (11232.12328::Double)

testUndefined = do
    x <- object
    -- set x "foo" (123::Int)
    u <- x % "foo" :: IO Int
    printRepr $! u

testFib = do
    printRepr $! toJsString $! show (fib 1000 :: Integer)
    where                    
        fib :: Int -> Integer

        fib n = go n (0,1)
          where
            go !n (!a, !b) | n==0      = a
                           | otherwise = go (n-1) (b, a+b)
        
        -- fib n = fibs !! n    
        -- fibs :: [Integer]
        -- fibs = [0,1] ++ zipWith (+) fibs (tail fibs)
        

testString = do
    printRepr $! "hans" `charAt` 0
    printRepr $! "hans" `charCodeAt` 0
    printRepr $! "hans" `lastIndexOf` "ans"
    printRepr $! "hans" `lastIndexOf` "anx"

testBool = do
    x <- object
    y <- create x
    printRepr $! (x `isPrototypeOf` y)
    printRepr $! (y `isPrototypeOf` x)

testLookup = do
    x <- object

    a <- x `hasProperty` "foo"
    printRepr $! a

    set x "foo" (int 1)

    b <- x `hasProperty` "foo"
    printRepr $! b

    r <- get x "foo" :: IO Int
    printRepr $! r

    delete x "foo"
    c <- x `hasProperty` "foo"
    printRepr $! c

testPropertyLookup = do
    x <- object
    y <- create x

    set x "foo" (int 1)

    a <- y `hasProperty` "foo"
    printRepr $! a
    b <- y `hasOwnProperty` "foo"
    printRepr $! b


setTimeout :: Int -> IO () -> IO ()
setTimeout t x = (unsafeGlobalLookup ["window"] %.. "setTimeout") (lift x) t

testLift = do
    g <- global

    as <- eval "([1,2,3])" :: IO JsArray
    let f = unsafeLift1 $ (+ 10) `fix1` int
    bs <- toObject as %. "map" $ f :: IO JsArray
    printRepr as
    printRepr bs

    cs <- eval "([5,5,6])" :: IO JsArray
    let add = unsafeLift2 ((+) `fix1` int)
    ds <- (toObject cs %.. "reduce") add (int 0) :: IO JsArray
    printRepr $! cs
    printRepr $! ds

    setTimeout 1000 $ printLog "Hello to Andersson!"
    setTimeout 2000 $ printLog "Hello to Pettersson!"
    setTimeout 3000 $ printLog "Hello to Lundström!"

testPrim = do
    printRepr $ False
    printRepr $ int 123
    printRepr $ str "foo"

    jf <- eval "(function(x){return x+x;})" :: IO JsFunction
    printRepr $ jf

    jo <- eval "({foo:123,bar:function(x){return x}})" :: IO JsObject
    printRepr $ jo

    let hf = \x -> x + x `with` int x
    printRepr $ hf

    let ho = (int 10, int 20)
    printRepr $ ho

testCall = do
    g <- global
    o <- eval "([1,2,3,4])"
    foo <- g % "foo"
    console <- g % "console" :: IO JsObject
    printRepr $ foo
    res <- call1 foo (o::JsObject)
    printRepr $ (res::JsObject)



testJQuery = do
    a <- query "#div1"
    fadeIn a
    fadeOut a
    b <- query "#div2"
    fadeInSlow b
    c <- query "#div3"
    fadeInDuring 2000 c



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


