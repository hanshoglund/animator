
{-# LANGUAGE 
    OverloadedStrings, NoMonomorphismRestriction, BangPatterns,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (null)
import Data.Foldable 
import Foreign.Ptr

import Haste.Prim(toPtr, fromPtr)
import Haste.Showable(show_)
-- import Animator.Prelude
import Animator.Internal.Prim

    
main = do
    testUndefined
    -- testFib
    -- testReadShow
    -- testBool
    -- testString
    -- testPrim
    -- testJQuery
    -- testLift  
    -- testLookup
    -- testPropertyLookup
    
{-# NOINLINE testUndefined #-}
{-# NOINLINE testFib #-}
{-# NOINLINE testReadShow #-}
{-# NOINLINE testBool #-}
{-# NOINLINE testString #-}
{-# NOINLINE testPrim #-}
{-# NOINLINE testJQuery #-}
{-# NOINLINE testLift #-}
{-# NOINLINE testLookup #-}


testReadShow = do
    printLog $ toJsString $ show_ (11232.12328::Double)
    printLog $ toJsString $ show  (11232.12328::Double)

testUndefined = do
    x <- object    
    -- set x "foo" (123::Int)
    u <- x %% "foo" :: IO Int
    printRepr $! u

testFib = do
    printRepr $! fib 30
    where
        fib !0 = 0
        fib !1 = 1
        fib !n = fib (n - 1) + fib (n - 2)

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
    
    set x "foo" (1::Int)

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

    set x "foo" (1::Int)
    
    a <- y `hasProperty` "foo"
    printRepr $! a
    b <- y `hasOwnProperty` "foo"
    printRepr $! b


withGlobal :: (JsObject -> IO a) -> IO a
withGlobal f = global >>= f

setTimeout :: Int -> IO () -> IO ()
setTimeout t x = withGlobal $ 
    \g -> (g %.. "setTimeout") (lift x) t
    
testLift = do
    g <- global

    as <- eval "([1,2,3])" :: IO JsArray
    let f = liftp1 ((+ 10) ::Int -> Int)
    bs <- toObject as %. "map" $ f :: IO JsArray
    printRepr as
    printRepr bs
    
    cs <- eval "([5,5,6])" :: IO JsArray
    let add = liftp2 ((+) :: Int -> Int -> Int) 
    ds <- (toObject cs %.. "reduce") add (0::Int) :: IO JsArray
    printRepr $! cs
    printRepr $! ds
    
    setTimeout 1000 $ printLog "Hello to Andersson!"
    setTimeout 2000 $ printLog "Hello to Pettersson!"
    setTimeout 3000 $ printLog "Hello to Lundström!"

testPrim = do    
    printRepr $ (False::Bool)
    printRepr $ (123::Int)
    printRepr $ ("foo"::JsString)

    jf <- eval "(function(x){return x+x;})" :: IO JsFunction
    printRepr $ jf

    jo <- eval "({foo:123,bar:function(x){return x}})" :: IO JsObject
    printRepr $ jo

    let hf = ((\x -> x + x) :: Int -> Int)
    printRepr $ hf

    let ho = (10::Int,20::Int)
    printRepr $ ho

testCall = do    
    g <- global                     
    o <- eval "([1,2,3,4])"
    foo <- g %% "foo"
    console <- g %% "console" :: IO JsObject
    printRepr $ foo
    res <- call1 foo (o::JsObject)
    printRepr $ (res::JsObject)



testJQuery = do
    g <- global
    jq <- g %% "jQuery"

    r1 <- call1 jq ("#div1"::JsString)
    r1 % "fadeIn" :: IO ()

    r2 <- call1 jq ("#div2"::JsString)
    r2 %. "fadeIn" $ ("slow"::JsString) :: IO ()

    r3 <- call1 jq ("#div3"::JsString)
    r3 %. "fadeIn" $ (5000::Double) :: IO ()


    -- object % "x" := 1
    --        % "y" := 2
    -- array [1,2,3] % "0" := 33