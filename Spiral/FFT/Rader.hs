{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.Rader
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.Rader (
    R,
    Perm(..),

    rader
  ) where

import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Array.Operators.IndexSpace
import Spiral.Array.Operators.Permute
import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL hiding (R)

-- | Rader DFT decomposition.
rader :: forall a . (RootOfUnity a, Show a) => Int -> a -> SPL a
rader p w = Pi (R p a) × (one ⊕ f_pm1 (1/u)) × d_p × (one ⊕ f_pm1 u) × Pi (R' p a')
  where
    a, a' :: Int
    a  = generator p
    a' = inv p a

    one :: SPL a
    one = spl $ matrix [[1]]

    u :: a
    u = omega (p-1)

    d_p :: SPL a
    d_p = spl (fromLists [[1, 1], [1, head deltas]]) ⊕ diag (tail deltas)

    f_pm1 :: a -> SPL a
    f_pm1 = RDFT (fromIntegral p-1)

    deltas :: [a]
    deltas = toList $
        fmap (/ (fromIntegral p-1)) $
        toMatrix (f_pm1 u) `mv` fromList [w^modExp a i p | i <- [0..p-2]]

data R

instance Permutation R where
    data Perm R = R  Int Int
                | R' Int Int
      deriving (Eq, Ord, Show)

    toPermute (R p a) = f
      where
        f :: Integral a => a -> a
        f 0 = 0
        f i = fromIntegral $ modExp a (i-1) p

    toPermute (R' p a) = bruteForceInvert (R p a)

    toBackpermute (R' p a) = f
      where
        f :: Integral a => a -> a
        f 0 = 0
        f i = fromIntegral $ modExp a (i-1) p

    toBackpermute (R p a) = bruteForceInvert (R' p a)

    dim (R  p _) = fromIntegral p
    dim (R' p _) = fromIntegral p

    invert pi = errordoc $ text "Cannot invert:" <+> ppr pi

instance Pretty (Perm R) where
    ppr (R  p a) = text "R_"  <> ppr p <> char '_' <> ppr a
    ppr (R' p a) = text "R'_" <> ppr p <> char '_' <> ppr a
