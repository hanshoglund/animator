
{-# LANGUAGE 
    NoMonomorphismRestriction,
    StandaloneDeriving,
    TypeFamilies #-}

module Animator.Animation
where

import Data.VectorSpace
newtype R2 = R2 { unR :: (Double, Double) }

instance AdditiveGroup R2 where
    zeroV                     = R2 (0,0)
    negateV (R2 (x,y))        = R2 (negate x, negate y)
    R2 (xa,ya) ^+^ R2 (xb,yb) = R2 (xa+xb, ya+yb)

instance VectorSpace R2 where
    type Scalar R2 = Double
    s *^ R2 (x,y) = R2 (s * x, s * y)

deriving instance Show R2
deriving instance Eq R2
deriving instance Ord R2

-- import qualified Data.List as L
-- import Data.Bits
-- 
-- data Color    = Color
-- data Gradient = Gradient
--         
-- data Point   = Point { getPoint :: (Double, Double) }
-- newtype Size = Size Point
-- newtype Pos  = Pos Point
-- 
-- data Time   = Double
-- type Sketch = Time -> Image
-- type Image  = Pos -> Color
-- 
-- -- at :: Pos -> Time -> Color
-- 
-- 
-- 
-- data Path      = Path
-- data Segment = Segment
-- data Curve      = Curve
--         
-- data Transformation = Transformation
-- -- identity
-- -- scale
-- -- shear
-- -- translate
-- -- rotate
-- 
-- -- runAnimation :: Backend -> Animation -> IO ()

-- bounds :: Bounded a => a -> (a, a)
-- bounds x = (minBound, maxBound)
-- 
-- boundsText :: (Bounded a, Show a) => a -> String -> String
-- boundsText x name = let (m, n) = bounds x
--     in name ++ " x => " ++ show m ++ " < x < " ++ show n
-- 

animatorVersion = [0, 56]
