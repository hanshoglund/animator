
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
import Unsafe.Coerce
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





















-- --------------------------------------------------------------------------------
-- Processing
-- --------------------------------------------------------------------------------

class JsRef a => HasGraphics a where
    toGraphics :: a -> Graphics
    toGraphics = unsafeCoerce

data Graphics
instance JsVal Graphics
instance JsRef Graphics
instance HasGraphics Graphics

data Processing
instance JsVal Processing
instance JsRef Processing
instance HasGraphics Processing

image :: Processing -> Graphics -> IO ()
image p x = (toObject p %... "image") x (0::Double) (0::Double)

-- --------------------------------------------------------------------------------
-- Shapes
-- --------------------------------------------------------------------------------

point :: HasGraphics a => a -> Double -> Double -> IO ()
point p x y = (toObject p %.. "point") x y 

line :: HasGraphics a => a -> Double -> Double -> Double -> Double -> IO ()
line p x1 y1 x2 y2 = (toObject p %.... "line") x1 y1 x2 y2

square :: HasGraphics a => a -> Double -> Double -> IO ()
square p x y = (toObject p %.... "rect") x y (1::Double) (1::Double)

circle :: HasGraphics a => a -> Double -> Double -> IO ()
circle p x y = (toObject p %.... "ellipse") x y (1::Double) (1::Double)

rect :: HasGraphics a => a -> Double -> Double -> Double -> Double -> IO ()
rect p x y w h = (toObject p %.... "rect") (x-w/2) (y-h/2) w h
-- change to get origin in the middle!

ellipse :: HasGraphics a => a -> Double -> Double -> Double -> Double -> IO ()
ellipse p x y w h = (toObject p %.... "ellipse") x y w h

-- arc
-- quad
-- triangle
-- curve
-- vertex
-- 3D


-- --------------------------------------------------------------------------------
-- Affine transformations
-- --------------------------------------------------------------------------------

translate :: HasGraphics a => a -> Double -> Double -> IO ()
translate p x y = (toObject p %.. "translate") x y

rotate :: HasGraphics a => a -> Double -> IO ()
rotate p x = (toObject p %. "rotate") x

scale :: HasGraphics a => a -> Double -> IO ()
scale p x = (toObject p %. "scale") x

-- reflect, shear, squeeze?


-- --------------------------------------------------------------------------------
-- Style and color
-- --------------------------------------------------------------------------------

size :: Processing -> Double -> Double -> IO ()
size p x y = (toObject p %.. "size") x y

data Color = Color Double Double Double Double
red         = Color 1.00 0.00 0.00 1
green       = Color 0.00 1.00 0.00 1
blue        = Color 0.00 0.00 1.00 1
black       = Color 0.00 0.00 0.00 1
white       = Color 0.00 0.00 0.00 1
transparent = Color 0.00 0.00 0.00 0

alpha :: Double -> Color
alpha x = Color 0 0 0 x

withOpacity :: Color -> Double -> Color 
withOpacity (Color r g b a) x = Color r g b x

atop :: Color -> Color -> Color
atop (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) = Color rx gx bx ax
    where 
        ax = 1 - (1 - a1) * (1 - a2)
        rx = r1 * a1 / ax + r2 * a2 * (1 - a1) / ax
        gx = g1 * a1 / ax + g2 * a2 * (1 - a1) / ax
        bx = b1 * a1 / ax + b2 * a2 * (1 - a1) / ax

-- internal
convertColor :: Color -> (Double, Double, Double, Double)
convertColor (Color r g b a) = (r*256, g*256, b*256, a*256)


background :: HasGraphics a => a -> Color -> IO ()
background p c = (toObject p %.... "background") r g b a
    where (r,g,b,a) = convertColor c

fill :: HasGraphics a => a -> Color -> IO ()
fill p c = (toObject p %.... "fill") r g b a
    where (r,g,b,a) = convertColor c

stroke :: HasGraphics a => a -> Color -> IO ()
stroke p c = (toObject p %.... "stroke") r g b a
    where (r,g,b,a) = convertColor c

-- --------------------------------------------------------------------------------
-- Events
-- --------------------------------------------------------------------------------

setMouseClicked :: Processing -> (Processing -> IO ()) -> IO ()
setMouseClicked p f = set (toObject p) "mouseClicked" (lift $ f p)

setMouseMoved :: Processing -> (Processing -> IO ()) -> IO ()
setMouseMoved p f = set (toObject p) "mouseMoved" (lift $ f p)

setDraw :: Processing -> (Processing -> IO ()) -> IO ()
setDraw p f = set (toObject p) "draw" (lift $ f p)

-- --------------------------------------------------------------------------------
-- Misc
-- --------------------------------------------------------------------------------

println :: Processing -> JsString -> IO () 
println p s = (toObject p %. "println") s

withGraphics :: Processing -> Double -> Double -> (Graphics -> IO ()) -> IO ()
withGraphics p x y f = do
    g <- (toObject p %... "createGraphics") x y ("P3D"::JsString)
    toObject g %% "beginDraw" :: IO ()
    f g
    toObject g %% "endDraw" :: IO ()
    image p g

runProcessing :: (Processing -> IO ()) -> JsString -> IO ()
runProcessing f n = do
    d <- document
    e <- getElementById d n
    p <- mkProcessing e (lift1 f) :: IO Processing
    return ()
    where
        mkProcessing = new2 $ unsafeGlobalLookup ["Processing"]

-- --------------------------------------------------------------------------------
-- Math
-- --------------------------------------------------------------------------------

tau = pi * 2





