module Animator.Animation
where

import qualified Data.List as L
import Data.Bits

data Color    = Color
data Gradient = Gradient
        
data Point   = Point { getPoint :: (Double, Double) }
newtype Size = Size Point
newtype Pos  = Pos Point

data Time   = Double
type Sketch = Time -> Image
type Image  = Pos -> Color

-- at :: Pos -> Time -> Color



data Path      = Path
data Segment = Segment
data Curve      = Curve
        
data Transformation = Transformation
-- identity
-- scale
-- shear
-- translate
-- rotate

-- runAnimation :: Backend -> Animation -> IO ()
animatorVersion = [0, 55]

-- Paper.js backend

