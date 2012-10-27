
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, MagicHash,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving,
    TypeSynonymInstances, FlexibleInstances, ForeignFunctionInterface, OverloadedStrings, CPP #-}

module Animator.Animation
where

newtype Time = Time { getTime :: Double }
    deriving (Eq, Ord, Show, Num, Fractional, Real, RealFrac, Floating)
    

-- Animation types
data Animation
    = AnPrim
    | AnComp  [Animation]
    | AnTrans Transformation Animation

-- Primitives
-- arc
-- ellipse
-- line
-- point
-- quad
-- rect
-- triangle

-- bezier
-- curve

-- TODO vertex/shape

-- Composition
onTop :: Animation -> Animation
onTop = error "Not implemented"


-- Style  
-- ColorStyle
--     background
--     fill
--     stroke
-- StrokeStyle
--     cap
--     join
--     weight





-- Transformations

data Transformation = Transformation

compose :: Transformation -> Transformation
compose = error "Not implemented"

translate :: Transformation
translate = error "Not implemented"
scale :: Transformation
scale = error "Not implemented"
rotate :: Transformation
rotate = error "Not implemented"
-- shear :: Transformation
-- shear = error "Not implemented"
-- squeeze :: Transformation
-- squeeze = error "Not implemented"

transform :: Transformation -> Animation -> Animation
transform = undefined


-- Dynamic

class Dynamic a where
    duration :: a -> Time
    instant  :: a -> Bool
    instant x = duration x <= 0

instance Dynamic Int where
    duration _ = 0
instance Dynamic Double where
    duration _ = 0