
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings, 
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction,
    BangPatterns #-}

module Main where

import Animator.Prelude
import Animator.Internal.Prim
import Foreign.Ptr
import Haste.Prim(toPtr, fromPtr)
import GHC.Prim
import Data.Foldable 
import Prelude hiding (null)
import qualified Data.Aeson

{-

FFI quirks:
    All imported js functions take and return extra parameter:
        f(..., _) {
            return [1,_, ...]
        }
    FFI functions unwrap one level of pointers (this include JsString and Int).
    This does not apply to callbacks!

Representation:

    Haskell value               JS value
    -----                       ------
    False :: Bool               false
    123 :: Int                  [1,123]
    "foo" :: JsString           [1,"foo"]

    eval "function(){...}"      [1,function(){...}]
    eval "{...}"                [1,{...}]

    \x->x+x :: Int->Int         function
    (10,20)::(Int,Int)          [1,[1,10],[1,20]]


Polymorhism requirements:
    Assignment and referencing
        x.n = v
        x.n
    Calls
        f(x,y,z)
-}

main = do
    testFib
    testJQuery
    -- testPrim
    -- testLift  
    -- testLookup
    -- testBool
    -- testString

testFib = do
    printRepr $! fib 10
    where
        fib !0 = 0
        fib !1 = 1
        fib !n = fib (n - 1) + fib (n - 2)

testString = do
    printRepr $! "hans" `charAt` 0
    printRepr $! "hans" `charCodeAt` 0    
    printRepr $! "hans" `lastIndexOf` "ans"
    printRepr $! "hans" `lastIndexOf` "anx"

testBool = do
    x <- object
    y <- create x
    let !r = isPrototypeOf x y
    let !u = isPrototypeOf y x
    printRepr $ (r, u)

testLookup = do
    x <- object
    set x "foo" (1::Int)
    y <- create x
    printLog $ show (x `isPrototypeOf` y)
    printLog $ show (y `isPrototypeOf` x)

withGlobal :: (JsObject -> IO a) -> IO a
withGlobal f = global >>= f

setTimeout :: Int -> IO () -> IO ()
setTimeout t x = withGlobal $ 
    \g -> (g %.. "setTimeout") (lift x) t
    
testLift = do
    g <- global

    as <- eval "([1,2,3])" :: IO JsObject
    let f = liftPure1 ((+ 10) ::Int -> Int)
    bs <- as %. "map" $ f :: IO JsArray
    printRepr as
    printRepr bs

    cs <- eval "([5,5,6])" :: IO JsObject
    let add = liftPure2 ((+) :: Int -> Int -> Int) 
    ds <- (cs %.. "reduce") add (0::Int) :: IO JsArray
    printRepr cs
    printRepr ds
    
    setTimeout 1000 $ printLog "Hello to Jonas!"


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