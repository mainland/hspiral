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
import Foreign.C.Types (CLLong)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.TypeLits (KnownNat,
                     natVal)
import Text.PrettyPrint.Mainland.Class

import Spiral.NumberTheory
import Spiral.RootOfUnity

instance Pretty i => Pretty (i `Mod` n) where
    pprPrec p z = pprPrec p (unMod z)

instance KnownNat p => Storable (ℤ/p) where
    sizeOf _ = sizeOf (undefined :: CLLong)

    alignment _ = alignment (undefined :: CLLong)

    peek p = toMod . fromIntegral <$> peek (castPtr p :: Ptr CLLong)

    poke p i = poke (castPtr p :: Ptr CLLong) (fromIntegral (unMod i))

instance KnownNat p => Fractional (ℤ/p) where
    recip = Data.Modular.inv

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance KnownNat p => RootOfUnity (ℤ/p) where
    omega n0
        | (p-1) `mod` n == 0 = g^((p-1) `div` n)
        | otherwise          = error $ "Cannot compute primitive " ++
                                       show n ++ "th root of unity in ℤ/" ++
                                       show p
      where
        p :: Integer
        p = natVal (Proxy :: Proxy p)

        g :: ℤ/p
        g = toMod (generator p)

        n :: Integer
        n = fromIntegral n0
