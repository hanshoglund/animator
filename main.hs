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
import Unsafe.Coerce
import Foreign.Ptr
import Haste.Prim(toPtr, fromPtr)
import GHC.Prim
import Data.Foldable

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

main = test

test = do    
    let jb = toPtr (False::Bool)
    logAny# $ unsafeCoerce jb

    let jn = toPtr (123::Int)
    logAny# $ unsafeCoerce jn

    let js = toPtr ("foo"::JsString)
    logAny# $ unsafeCoerce js

    jf <- fmap toPtr $ eval "(function(x){return x+x;})"
    logAny# $ unsafeCoerce jf

    jo <- fmap toPtr $ eval "({foo:123,bar:function(x){return x}})"
    logAny# $ unsafeCoerce jo

    let hf = toPtr ((\x -> x + x) :: Int -> Int)
    logAny# $ unsafeCoerce hf

    let ho = toPtr (10::Int,20::Int)
    logAny# $ unsafeCoerce ho





    -- a <- object
    -- set "x" a (1::Float)
    -- set "y" a (2::Float)
    --
    -- b <- object
    -- set "x" b (1::Float)
    -- set "y" b (2::Float)
    -- logAny# $ unsafeCoerce a
    -- logAny# $ unsafeCoerce b
    --
    -- xa <- get "x" a :: IO Float
    -- xb <- get "x" b :: IO Float
    -- ya <- get "y" a :: IO Float
    -- yb <- get "y" b :: IO Float
    -- logAny# $ unsafeCoerce $ (xa * xb + ya * yb)

    -- o <- object
    -- logAny# $ (getJsObject $ o)



    -- object % "x" := 1
    --        % "y" := 2
    -- array [1,2,3] % "0" := 33