-- https://github.com/yogsototh/mandelbrot/blob/master/mandelbrot.hs

module Complex where

import Foreign.C.Types

type R = CFloat

data Complex = C {-# UNPACK #-} !R {-# UNPACK #-} !R deriving (Show,Eq)
instance Num Complex where
    fromInteger n = C  (fromIntegral n) 0.0
    (C x y) * (C z t) = C (z*x - y*t)  (y*z + x*t)
    (C x y) + (C z t) = C (x+z) (y+t)
    abs (C x y)     = C (sqrt (x*x + y*y)) 0.0
    signum (C x y)  = C (signum x) 0.0

real :: Complex -> R
real (C x y)    = x

im :: Complex -> R
im   (C x y)    = y

magnitude :: Complex -> R
magnitude = real.abs

