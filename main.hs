{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies #-}

module Main where              

import Animator.Prelude
import Animator.Internal.Prim

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs = map fib [0..14]




main = do
    x <- object
    set "foo" x ("1232"::String)
    r <- (get "foo" x :: IO String)
    windowDocumentWrite $ (show r)

    -- y <- new
    -- y %%. "foo" .= "foo"
    -- y %%. "bar" .= "bar"
    -- y %%. "baz" .= "baz"
    -- a2 <- (y %. "foo")
    -- b2 <- (y %. "bar")
    -- c2 <- (y %. "baz")
    -- documentWrite $ show ((a2,b2,c2) :: (String,String,String))

    -- windowAlert "This is a warning"
    -- windowConsoleLog "This goes in the log"
    -- windowConsoleLog $ "Fibs is " ++ show fibs
    windowDocumentWrite "This goes in the doc"          

    -- global %. "window" %. "console" %. "log"
