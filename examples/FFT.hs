{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Complex
import Text.PrettyPrint.Mainland

import SPL.ExtendedFloat
import SPL.Exp
import SPL.Syntax

main :: IO ()
main = do
    putDocLn (pprMatrix (w 2 2 :: SPL (Exp (Complex Double))))
    putDocLn (pprMatrix (t 4 2 :: SPL (Exp (Complex Double))))
    putDocLn (ppr (f 4 :: SPL (Exp (Complex Double))))
    putDocLn (pprMatrix (f 4 :: SPL (Exp (Complex Double))))

-- | The $W_m(\omega_n)$ matrix
w :: forall e . ExtendedFloat e => Int -> Int -> SPL e
w m n = D (m, m) f
  where
    f :: Ix -> e
    f (i, j) | i == j    = omega n^i
             | otherwise = 0

-- | Twiddle factor matrix $T^{mn}_n$
t :: ExtendedFloat e => Int -> Int -> SPL e
t mn n = I m ⊕ go (n-1)
  where
    m = mn `quot` n

    go i | i == 1    = w m mn
         | otherwise = w m mn ⊕ go (i-1)

-- | DFT matrix $F_n$, for $n$ even
f :: ExtendedFloat e => Int -> SPL e
f 2 = matrix [[1,  1],
              [1, -1]]

f n | even n =
    (f 2 ⊗ I n2) × t n n2 × (I 2 ⊗ f n2) × L n 2
  where
    n2 = n `quot` 2

f n =
    error $ "f: not even: " ++ show n
