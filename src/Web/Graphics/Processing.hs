
{-# LANGUAGE MagicHash, CPP, ForeignFunctionInterface, OverloadedStrings,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    NoMonomorphismRestriction, EmptyDataDecls #-}

-------------------------------------------------------------------------------------

-- |
-- Module      : Web.Graphics.Processing
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : Haste
--
-- Bindings for Processing.js 1.4.1.
--

-------------------------------------------------------------------------------------

module Web.Graphics.Processing -- (
--  )
where

import Prelude hiding (lookup)
import Data.Colour
import Data.Colour.SRGB
import Web.Document
import Foreign.JavaScript -- hiding (take, drop, slice, length)


data Processing
instance JsVal Processing
instance JsRef Processing




size :: Processing -> Double -> Double -> IO ()
size p x y = (toObject p %.. "size") x y

background :: Processing -> Color -> IO ()
background p c = (toObject p %.... "background") r g b a
    where (r,g,b,a) = convertColor c

setMouseClicked :: Processing -> IO () -> IO ()
setMouseClicked p f = set (toObject p) "mouseClicked" (lift f)

setMouseMoved :: Processing -> IO () -> IO ()
setMouseMoved p f = set (toObject p) "mouseMoved" (lift f)

setDraw :: Processing -> IO () -> IO ()
setDraw p f = set (toObject p) "draw" (lift f)


runProcessing :: (Processing -> IO ()) -> JsString -> IO ()
runProcessing f n = do
    d <- document
    e <- getElementById d n
    p <- mkProcessing e (lift1 f) :: IO Processing
    return ()
    where
        mkProcessing = new2 $ unsafeGlobalLookup ["Processing"]






data Color = Color Double Double Double Double
red   = Color 1 0 0 1
green = Color 0 1 0 1
blue  = Color 0 0 1 1

convertColor :: Color -> (Double, Double, Double, Double)
convertColor (Color r g b a) = (r*256, g*256, b*256, a*256)

-- convertColor :: AlphaColour Double -> (Double, Double, Double, Double)
-- convertColor c = (r,g,b,a)
--     where
--         rgb = toSRGB (c `over` black)
--         r   = channelRed rgb
--         g   = channelGreen rgb
--         b   = channelBlue rgb
--         a   = alphaChannel c
