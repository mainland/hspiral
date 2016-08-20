-- |
-- Module      :  SPL.ExtendedFloat
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.ExtendedFloat (
    ExtendedFloat(..),

    omega
  ) where

import Data.Complex
import Data.Ratio

-- | Function for expression quantities that may have more precise
-- representations.
class Num a => ExtendedFloat a where
    -- | $e^{2 \pi i \frac{k}{n}$
    rootOfUnity :: Rational -> a

    -- | Normalize a root of unity
    normRootOfUnity :: a -> a
    normRootOfUnity x = x

instance RealFloat a => ExtendedFloat (Complex a) where
    rootOfUnity r = exp (2*pi*i*fromRational r)
      where
        i = 0:+1

-- | $e^{\frac{-2 \pi i}{n}$
omega :: (Integral a, ExtendedFloat b) => a -> b
omega n = rootOfUnity (-1 % fromIntegral n)
