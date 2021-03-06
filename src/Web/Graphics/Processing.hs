
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

module Web.Graphics.Processing (

        -- ** Signals
        Signal,
        liftIO,
        mouseS,
        timeS,

        -- ** Animation
        Animation,
        squareA,
        circleA,
        scaleA,
        fillA,
        strokeA,

        -- ** Colors
        Color,
        red,
        green,
        blue,
        black,
        white,
        transparent,
        alpha,
        atop,
        withOpacity,

        -- ** Math
        tau,

        -- ** Run
        runAnimation,
)
where

import Prelude hiding (lookup)
import Data.Semigroup
import Control.Applicative
import Control.Monad (join, ap)

import Web.Document
import Unsafe.Coerce
import Foreign.JavaScript hiding (join)

-- TODO make variable
kSize      = 600
kFrameRate = 15

newtype Signal a = Signal { getSignal :: Processing -> IO a }
instance Functor Signal where
    fmap f (Signal s) = Signal (fmap f . s)
instance Applicative Signal where
    pure  = return
    (<*>) = ap
instance Monad Signal where
    return         = Signal . const . return
    Signal f >>= g = Signal $ \p -> (f $ p) >>= \x -> (getSignal (g x) $ p)

instance Num a => Num (Signal a) where
    (+)         = liftA2 (+)
    (*)         = liftA2 (*)
    (-)         = liftA2 (-)
    negate      = liftA negate
    abs         = liftA abs
    signum      = liftA signum
    fromInteger = pure . fromInteger
instance Fractional a => Fractional (Signal a) where
    (/)         = liftA2 (/)
    recip       = liftA recip
    fromRational = pure . fromRational
instance Floating a => Floating (Signal a) where
    pi          = pure pi
    exp         = liftA exp
    sqrt        = liftA sqrt
    log         = liftA log
    (**)        = liftA2 (**)
    logBase     = liftA2 logBase
    sin         = liftA sin
    tan         = liftA tan
    cos         = liftA cos
    asin        = liftA asin
    atan        = liftA atan
    acos        = liftA acos
    sinh        = liftA sinh
    tanh        = liftA tanh
    cosh        = liftA cosh
    asinh       = liftA asinh
    atanh       = liftA atanh
    acosh       = liftA acosh
-- instance Integral a => Integral (Signal a) where
    -- quot        = liftA2 quot
    -- rem :: a -> a -> a
    -- div :: a -> a -> a
    -- mod :: a -> a -> a
    -- quotRem :: a -> a -> (a, a)
    -- divMod :: a -> a -> (a, a)
    -- toInteger :: a -> Integer






runSignal :: Signal a -> Processing -> IO a
runSignal (Signal f) p = f p

liftIO :: IO a -> Signal a
liftIO f = Signal (\_ -> f)



mouseS :: Signal (Double, Double)
mouseS = Signal mouse

timeS :: Signal Double
timeS = Signal frame

newtype Animation = Animation { getAnimation :: [Layer] }
data Layer
    = Still DrawInstr
    | Anim  (Signal DrawInstr)
anim :: Animation -> Animation
anim = Animation . map animL . getAnimation
animL :: Layer -> Layer
animL (Still d) = Anim (pure d)

instance Semigroup Animation where
    (Animation a) <> (Animation b) = Animation (a <> b)
instance Monoid Animation where
    mempty  = Animation []
    mappend = (<>)


squareA :: Animation
squareA = Animation [Still square]

circleA :: Animation
circleA = Animation [Still circle]

-- translateA :: Signal Double -> Signal Double -> Animation -> Animation
-- rotate :: Signal Double -> Animation -> Animation

scaleA :: Signal Double -> Animation -> Animation
scaleA x (Animation ls) = Animation $ fmap (scaleL x) ls

scaleL :: Signal Double -> Layer -> Layer
scaleL x (Still d) = scaleL x (animL $ Still d)
scaleL x (Anim d) = Anim $ liftA2 (\x d g -> scale g x >> d g) x d


-- scaleXY :: Signal Double -> Signal Double -> Animation -> Animation
-- scaleX :: Signal Double -> Animation -> Animation
-- scaleY :: Signal Double -> Animation -> Animation

fillA :: Signal Color -> Animation -> Animation
fillA x (Animation ls) = Animation $ fmap (fillL x) ls

fillL :: Signal Color -> Layer -> Layer
fillL x (Still d) = fillL x (animL $ Still d)
fillL x (Anim d) = Anim $ liftA2 (\x d g -> fill g x >> d g) x d

strokeA :: Signal Color -> Animation -> Animation
strokeA x (Animation ls) = Animation $ fmap (strokeL x) ls

strokeL :: Signal Color -> Layer -> Layer
strokeL x (Still d) = strokeL x (animL $ Still d)
strokeL x (Anim d) = Anim $ liftA2 (\x d g -> stroke g x >> d g) x d

















-- | Instructions to draw an image at size (1,1) with origin at (0,0)
type DrawInstr = Graphics -> IO ()

-- TODO also flip Y
renderDrawInstr :: Double -> DrawInstr -> DrawInstr
renderDrawInstr x draw g = do
    stroke g transparent
    fill g red
    translate g (x/2) (x/2)
    scale g x
    mirrorY g
    draw g

-- Render at a point in time
-- TODO optimize
renderAnimation :: Processing -> Animation -> IO ()
renderAnimation p (Animation ls) = do
    background p white
    size p kSize kSize
    frameRate p kFrameRate
    mapM (renderLayer p) ls
    return ()

renderLayer :: Processing -> Layer -> IO ()
renderLayer p (Still d) = do
    withGraphics p kSize kSize $ renderDrawInstr kSize d
renderLayer p (Anim ds) = do
    d <- runSignal ds p
    withGraphics p kSize kSize $ renderDrawInstr kSize d


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

square :: HasGraphics a => a -> IO ()
square p = rect p 0 0 1 1

circle :: HasGraphics a => a -> IO ()
circle p = ellipse p 0 0 1 1

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

scaleXY :: HasGraphics a => a -> Double -> Double -> IO ()
scaleXY p x y = (toObject p %.. "scale") x y

scaleX :: HasGraphics a => a -> Double -> IO ()
scaleX g x = scaleXY g x 1

scaleY :: HasGraphics a => a -> Double -> IO ()
scaleY g y = scaleXY g 1 y

mirrorX :: HasGraphics a => a -> IO ()
mirrorX g = scaleX g (-1)

mirrorY :: HasGraphics a => a -> IO ()
mirrorY g = scaleY g (-1)

-- reflect, shear, squeeze?


-- --------------------------------------------------------------------------------
-- Style and color
-- --------------------------------------------------------------------------------

data Color = Color Double Double Double Double
red         = Color 1.00 0.00 0.00 1
green       = Color 0.00 1.00 0.00 1
blue        = Color 0.00 0.00 1.00 1
black       = Color 0.00 0.00 0.00 1
white       = Color 1.00 1.00 1.00 1
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
-- Events and input
-- --------------------------------------------------------------------------------

frame :: Processing -> IO Double
frame p = toObject p % "frameCount"

mouse :: Processing -> IO (Double, Double)
mouse p = do
    x <- mouseX p
    y <- mouseY p
    return (x,y)

mouseX :: Processing -> IO Double
mouseX p = toObject p % "mouseX"

mouseY :: Processing -> IO Double
mouseY p = toObject p % "mouseY"

setMouseClicked :: Processing -> (Processing -> IO ()) -> IO ()
setMouseClicked p f = set (toObject p) "mouseClicked" (lift $ f p)

setMouseMoved :: Processing -> (Processing -> IO ()) -> IO ()
setMouseMoved p f = set (toObject p) "mouseMoved" (lift $ f p)

setDraw :: Processing -> (Processing -> IO ()) -> IO ()
setDraw p f = set (toObject p) "draw" (lift $ f p)

-- --------------------------------------------------------------------------------
-- Misc
-- --------------------------------------------------------------------------------

size :: Processing -> Double -> Double -> IO ()
size p x y = (toObject p %.. "size") x y

frameRate :: Processing -> Double -> IO ()
frameRate p x = (toObject p %. "frameRate") x

println :: Processing -> JsString -> IO ()
println p s = (toObject p %. "println") s

withGraphics :: Processing -> Double -> Double -> (Graphics -> IO ()) -> IO ()
withGraphics p x y f = do
    g <- (toObject p %... "createGraphics") x y ("P3D"::JsString)
    toObject g %% "beginDraw" :: IO ()
    f g
    toObject g %% "endDraw" :: IO ()
    image p g

runAnimation :: Animation -> JsString -> IO ()
runAnimation anim canvas = runProcessing h canvas
    where
        h p = setDraw p (flip renderAnimation $ anim)

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





