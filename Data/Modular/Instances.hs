{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Data.Modular.Instances
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Data.Modular.Instances () where

import Data.Modular
import Data.Proxy (Proxy (..))
import Data.Ratio
import GHC.TypeLits (KnownNat,
                     natVal)
import Text.PrettyPrint.Mainland.Class

import Spiral.NumberTheory
import Spiral.RootOfUnity

instance Pretty i => Pretty (i `Mod` n) where
    pprPrec p z = pprPrec p (unMod z)

instance KnownNat p => Fractional (ℤ/p) where
    recip = Data.Modular.inv

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance KnownNat p => RootOfUnity (ℤ/p) where
    omega n0
        | (p-1) `mod` n == 0 = g^((p-1) `div` n)
        | otherwise          = error "Cannot compute primitive root of unity"
      where
        p :: Integer
        p = natVal (Proxy :: Proxy p)

        g :: ℤ/p
        g = toMod (generator p)

        n :: Integer
        n = fromIntegral n0
