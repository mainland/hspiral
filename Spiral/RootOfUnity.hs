-- |
-- Module      :  Spiral.RootOfUnity
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.RootOfUnity (
    RootOfUnity(..)
  ) where

import Data.Complex
import Data.Complex.Cyclotomic

-- | Types that have primitive roots of unity.
class Num a => RootOfUnity a where
    -- | @rootOfUnity n k@ computes the primitive @n@th root of unity raised to
    -- the @k@th power.
    rootOfUnity :: Int -> Int -> a

    -- | Computes the primitive @n@th root of unity.
    omega :: Int -> a
    omega n = rootOfUnity n 1

instance RealFloat a => RootOfUnity (Complex a) where
    -- | $e^{-2 \pi i \frac{k}{n}$
    rootOfUnity n k = exp (-2*pi*i/fromIntegral n)^^k
      where
        i = 0:+1

instance RootOfUnity Cyclotomic where
    -- 'Data.Complex.Cyclotomic.e' computes $e^{2 \pi i/n}$, so we need to take
    -- the reciprocal.
    rootOfUnity n k = (1/e (fromIntegral n))^^k
