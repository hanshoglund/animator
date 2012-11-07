
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
import Control.Applicative
import Control.Monad (join, ap)
import Data.Colour
import Data.Colour.SRGB

import Web.Document
import Foreign.JavaScript hiding (join)


newtype S a = S { getS :: Processing -> IO a }
instance Functor S where
    fmap f (S s) = S (fmap f . s)
instance Applicative S where
    pure  = return
    (<*>) = ap
instance Monad S where             
    return    = S . const . return
    S f >>= g = S $ \p -> fap p . getS . g =<< fap p f
        where
            fap x f = f x

liftIO :: IO a -> S a
liftIO f = S (\_ -> f)            





















data Processing
instance JsVal Processing
instance JsRef Processing


-- --------------------------------------------------------------------------------
-- Shapes
-- --------------------------------------------------------------------------------

point :: Processing -> Double -> Double -> IO ()
point p x y = (toObject p %.. "point") x y 

line :: Processing -> Double -> Double -> Double -> Double -> IO ()
line p x1 y1 x2 y2 = (toObject p %.... "line") x1 y1 x2 y2

square :: Processing -> Double -> Double -> IO ()
square p x y = (toObject p %.... "rect") x y (1::Double) (1::Double)

circle :: Processing -> Double -> Double -> IO ()
circle p x y = (toObject p %.... "ellipse") x y (1::Double) (1::Double)

rect :: Processing -> Double -> Double -> Double -> Double -> IO ()
rect p x y w h = (toObject p %.... "rect") x y w h

ellipse :: Processing -> Double -> Double -> Double -> Double -> IO ()
ellipse p x y w h = (toObject p %.... "ellipse") x y w h

-- arc
-- quad
-- triangle
-- curve
-- vertex
-- 3D


-- --------------------------------------------------------------------------------
-- Shapes
-- --------------------------------------------------------------------------------




size :: Processing -> Double -> Double -> IO ()
size p x y = (toObject p %.. "size") x y

background :: Processing -> Color -> IO ()
background p c = (toObject p %.... "background") r g b a
    where (r,g,b,a) = convertColor c

fill :: Processing -> Color -> IO ()
fill p c = (toObject p %.... "fill") r g b a
    where (r,g,b,a) = convertColor c

setMouseClicked :: Processing -> (Processing -> IO ()) -> IO ()
setMouseClicked p f = set (toObject p) "mouseClicked" (lift $ f p)

setMouseMoved :: Processing -> (Processing -> IO ()) -> IO ()
setMouseMoved p f = set (toObject p) "mouseMoved" (lift $ f p)

setDraw :: Processing -> (Processing -> IO ()) -> IO ()
setDraw p f = set (toObject p) "draw" (lift $ f p)

println :: Processing -> JsString -> IO () 
println p s = (toObject p %. "println") s


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
