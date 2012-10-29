{-# LANGUAGE
    NoMonomorphismRestriction,
    TypeFamilies,
    ForeignFunctionInterface,
    OverloadedStrings,
    BangPatterns,
    MagicHash,
    UnboxedTuples #-}

module Main where

import Animator.Prelude
import Animator.Internal.Prim
import Foreign.Ptr
import Haste.Prim(toPtr, fromPtr)
import GHC.Prim
import Data.Foldable 
import Prelude hiding (null)

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
    testJQuery
    testLift
    
testLift = do
    g <- global

    as <- eval "([1,2,3])" :: IO JsObject
    -- f  <- eval "(function(x) { return x + 10 })" :: IO JsFunction
    -- let f = lift1' ((\x -> return $ x + 10)::Int -> IO Int)
    let f = lift1 ((\x -> x + 10)::Int -> Int)
    bs <- invoke1 as "map" f :: IO JsArray
    printRepr as
    printRepr bs

    cs <- eval "([5,5,6])" :: IO JsObject
    -- h  <- eval "(function(x,y) { return x + y; })" :: IO JsFunction
    let h = lift2 ((\x y -> x + y) :: Int -> Int -> Int)
    ds <- invoke2 cs "reduce" h (0::Int) :: IO JsArray
    printRepr cs
    printRepr ds

    
    
    st <- get g "setTimeout"
    f <- eval "(function(){ console.log('Hello!') })" :: IO JsFunction
    let f = lift' $ printLog "Hello from Haskell!"
    call2 st f (1000::Int) :: IO ()

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
    foo <- get g "foo"
    console <- get g "console" :: IO JsObject
    printRepr $ foo
    res <- call1 foo (o::JsObject)
    printRepr $ (res::JsObject)


testJQuery = do
    g <- global
    jq <- get g "jQuery"

    r1 <- call1 jq ("#div1"::JsString)
    r1 %% "fadeIn" :: IO ()

    r2 <- call1 jq ("#div2"::JsString)
    (r2 %%! "fadeIn") ("slow"::JsString) :: IO ()

    r3 <- call1 jq ("#div3"::JsString)
    (r3 %%! "fadeIn") (5000::Double) :: IO ()


    -- object % "x" := 1
    --        % "y" := 2
    -- array [1,2,3] % "0" := 33