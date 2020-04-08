{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.FFT.Rader
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.FFT.Rader (
    rader,

    raderII,
    raderIII
  ) where

import qualified Spiral.Array as A
import qualified Spiral.Array.Operators.Matrix as A
import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL

-- | Rader DFT decomposition.
rader :: forall a . (RootOfUnity a, Show a) => Int -> a -> SPL a
rader p w = permute (R p a) × (one ⊕ f_pm1 (1/u)) × d_p × (one ⊕ f_pm1 u) × backpermute (R p a')
  where
    a, a' :: Int
    a  = generator p
    a' = inv p a

    one :: SPL a
    one = fromLists [[1]]

    u :: a
    u = omega (p-1)

    d_p :: SPL a
    d_p = fromLists [[1, 1], [1, head deltas]] ⊕ diag (tail deltas)

    f_pm1 :: a -> SPL a
    f_pm1 = F (fromIntegral p-1)

    deltas :: [a]
    deltas = A.toList $
        fmap (/ (fromIntegral p-1)) $
        toMatrix (f_pm1 u) A.#> A.fromList [w^modExp a i p | i <- [0..p-2]]

-- | Variant II of Rader's Algorithm that explicitly factors out pre-additions
-- | Taken from Tolimieri's Algorithms for DFT and Convolution (Chapter 9)
raderII :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
raderII p w = permute (R p a) × fundamental_factor × backpermute (R p a)
  where
    one :: SPL a
    one = fromLists [[1]]

    a :: Int
    a  = generator p

    omegas :: [a]
    omegas = [w^modExp a i p | i <- [0..p-2]]

    fundamental_factor :: SPL a
    fundamental_factor = (one ⊕ DFT (p-1)) × (one ⊕ dp) × b_mat × (one ⊕ DFT (p-1))

    dp :: SPL a
    dp = matrix $ toMatrix (DFT' (p-1) × circulant × DFT' (p-1))

    circulant :: SPL a
    circulant = skew omegas

    b_mat :: SPL a
    b_mat = fromLists [[1,                   1],
                       [-fromIntegral (p-1), 1]]
            ⊕ I (p-2)

-- | Variant III of Rader's Algorithm that explicitly factors out pre-additions
-- | Taken from Tolimieri's Algorithms for DFT and Convolution (Chapter 9)
raderIII :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
raderIII p w = permute (R p a) × fundamental_factor × backpermute (R p a)
 where
   a :: Int
   a  = generator p

   one :: SPL a
   one = fromLists [[1]]

   pm1_2 :: Int
   pm1_2 = (p - 1) `div` 2

   n_epm1, epm1t :: SPL a
   n_epm1 = fromLists $ replicate (p-1) [-1]
   epm1t  = fromLists [replicate (p-1) 1]

   omegas :: [a]
   omegas = [w^modExp a i p | i <- [0..p-2]]

   yp :: SPL a
   yp = matrix $ toMatrix (fp' × circulant × fp')

   fp, fp' :: SPL a
   fp  = (F2 ⊗ I pm1_2)
   fp' = (DFT' 2 ⊗ I pm1_2)

   fundamental_factor :: SPL a
   fundamental_factor = (one ⊕ fp) × (one ⊕ yp) × (one ⊕ fp) × a_mat

   circulant :: SPL a
   circulant = skew omegas

   a_mat :: SPL a
   a_mat = block one    epm1t
                 n_epm1 (I (p-1))
