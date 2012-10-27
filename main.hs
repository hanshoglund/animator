{-# LANGUAGE 
    NoMonomorphismRestriction,
    TypeFamilies,
    ForeignFunctionInterface,
    OverloadedStrings,
    MagicHash #-}

module Main where              

import Animator.Prelude
import Animator.Internal.Prim
import Unsafe.Coerce
import Foreign.Ptr
import Haste.Prim(toPtr)



main = do
    -- x <- object
    -- set "foo" x (1232::Int)
    -- r <- (get "foo" x :: IO JsObject)
    -- logAny# $ getJsObject r                    
    
    -- let p = toPtr ((\x -> x + x) :: Int -> Int)
    o <- object
    set "x" o (1::Int)
    logAny# $ unsafeCoerce (getJsObject $ o)

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

           
    -- object % "x" := 1 
    --        % "y" := 2
    -- array [1,2,3] % "0" := 33