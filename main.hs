{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies #-}

module Main where              

import Animator.Prelude
import Animator.Internal.Prim


-- 
main = do
    x <- new
    set "foo" x (1::Int)
    set "bar" x (1::Int)
    set "baz" x (1::Int)
    a <- get "foo" x
    b <- get "bar" x
    c <- get "baz" x
    documentWrite $ show ((a,b,c) :: (Int,Int,Int))

    -- y <- new
    -- y %%. "foo" .= "foo"
    -- y %%. "bar" .= "bar"
    -- y %%. "baz" .= "baz"
    -- a2 <- (y %. "foo")
    -- b2 <- (y %. "bar")
    -- c2 <- (y %. "baz")
    -- documentWrite $ show ((a2,b2,c2) :: (String,String,String))

    -- alert "This is a warning"
    consoleLog "This goes in the log"
    documentWrite "This goes in the doc"          

    -- global %. "window" %. "console" %. "log"
