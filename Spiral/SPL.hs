{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.SPL
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.SPL (
    SPL(..),
    spl,
    lperm,
    splExtent,
    toMatrix,

    (⊗),
    (×),
    (⊕),
  ) where

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.ExtendedFloat
import Spiral.Util.Pretty

-- | An SPL transform. The type index is the type of the scalar values in the
-- transformed vector.
data SPL a where
    -- An "embedded" array with an unknown representation.
    E :: (IArray r DIM2 e, Pretty (Array r DIM2 e))
      => Array r DIM2 e
      -> SPL e

    -- The $n \times n$ identity matrix.
    I :: Int -> SPL e

    -- The $n \times n$ reverse identity matrix.
    J :: Int -> SPL e

    -- The rotation matrix
    R :: Floating a => a -> SPL a

    -- The $L^{rs}_s$ $rs \times rs$ stride permutation matrix with stride $s$.
    L :: Int -> Int -> SPL e

    -- Kronecker product
    Kron :: SPL e -> SPL e -> SPL e

    -- Direct sum
    DSum :: SPL e -> SPL e -> SPL e

    -- Matrix product
    Prod :: SPL e -> SPL e -> SPL e

    -- The 2x2 DFT
    F2 :: SPL e

    -- The nxn DFT matrix
    DFT :: ExtendedFloat a => Int -> SPL a

-- | Embed any 'Array' as an SPL term.
spl :: (IArray r DIM2 e, Pretty (Array r DIM2 e))
    => Array r DIM2 e
    -> SPL e
spl = E

-- | The /inverse/ permutation $L^{mn}_n$. Useful for 'backpermute'.
lperm :: Integral a => a -> a -> a -> a
lperm mn n i = i*n `mod` mn + i*n `div` mn

splExtent :: SPL a -> DIM2
splExtent (E a)     = extent a
splExtent (I n)     = ix2 n n
splExtent (J n)     = ix2 n n
splExtent (R _)     = ix2 2 2
splExtent (L mn _n) = ix2 mn mn

splExtent (Kron a b) = ix2 (m*p) (n*q)
  where
    Z :. m :. n = splExtent a
    Z :. p :. q = splExtent b

splExtent (DSum a b) = ix2 (m+p) (n+q)
  where
    Z :. m :. n = splExtent a
    Z :. p :. q = splExtent b

splExtent (Prod a b) = ix2 m q
  where
    Z :. m  :. _n = splExtent a
    Z :. _p :. q  = splExtent b

splExtent F2 = ix2 2 2

splExtent (DFT n) = ix2 n n

toMatrix :: forall e . Num e => SPL e -> Matrix M e
toMatrix (E a) =
    manifest a

toMatrix (Kron a b) =
    M (ix2 (m*p) (n*q)) $ V.generate (m*p*n*q) f
  where
    Z :. m :. n = extent a'
    Z :. p :. q = extent b'

    a' = toMatrix a
    b' = toMatrix b

    f :: Int -> e
    f k = a' ! ix2 (i `quot` p) (j `quot` q) *
          b' ! ix2 (i `rem` p) (j `rem` q)
      where
        (i, j) = k `quotRem` (n*q)

toMatrix (DSum a b) =
    M (ix2 (m+p) (n+q)) $ V.generate ((m+p)*(n+q)) f
  where
    Z :. m :. n = extent a'
    Z :. p :. q = extent b'

    a' = toMatrix a
    b' = toMatrix b

    f :: Int -> e
    f k | i < m  && j < n  = a' ! ix2 i j
        | i >= m && j >= n = b' ! ix2 (i-m) (j-n)
        | otherwise        = 0
      where
        (i, j) = k `quotRem` (n+q)

toMatrix (Prod a b) =
    M (ix2 m q) $ V.generate (m*q) f
  where
    Z :. m  :. n = extent a'
    Z :. _p :. q  = extent b'

    a' = toMatrix a
    b' = toMatrix b

    f :: Int -> e
    f k = V.sum $ V.zipWith (*) (row a' i) (col b' j)
      where
        (i, j) = k `quotRem` n

toMatrix (I n) =
    manifest $ fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0

toMatrix (J n) =
    manifest $ fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i + j == n = 1
                    | otherwise  = 0

toMatrix (R alpha) =
    matrix [[cos alpha, -(sin alpha)],
            [sin alpha, cos alpha]]

toMatrix (L mn n) =
    manifest $ fromFunction (ix2 mn mn) f
  where
    f (Z :. i :. j) | j == i*n `mod` mn + i*n `div` mn = 1
                    | otherwise                        = 0

toMatrix F2 =
    matrix [[1,  1],
            [1, -1]]

toMatrix (DFT n) = manifest $ fromFunction (ix2 n n) f
  where
    f :: forall a . ExtendedFloat a => DIM2 -> a
    f (Z :. i :. j) = w ^ (i*j)
      where
        w :: a
        w = omega n

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec p (E a)      = pprPrec p a
    pprPrec _ (I n)      = text "I_" <> ppr n
    pprPrec _ (J n)      = text "J_" <> ppr n
    pprPrec _ (R alpha)  = text "R_" <> ppr alpha
    pprPrec _ (L rs s)   = text "L^" <> ppr rs <> char '_' <> ppr s
    pprPrec p (Kron a b) = infixop p KOp a b
    pprPrec p (DSum a b) = infixop p DSOp a b
    pprPrec p (Prod a b) = infixop p POp a b
    pprPrec _ F2         = text "F_2"
    pprPrec _ (DFT n)    = text "DFT_" <> ppr n

data MatrixBinop = KOp
                 | DSOp
                 | POp
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
    fixity KOp  = infixl_ 7
    fixity DSOp = infixl_ 6
    fixity POp  = infixl_ 7

instance Pretty MatrixBinop where
    ppr KOp  = char '⊗'
    ppr DSOp = char '⊕'
    ppr POp  = char '×'

-- | Extract a row of a matrix
row :: Num e => Matrix M e -> Int -> V.Vector e
row e i = V.slice (i*n) n xs
  where
    M (Z :. _m :. n) xs = manifest e

-- | Extract a column of a matrix
col :: forall e . Num e => Matrix M e -> Int -> V.Vector e
col e j = V.generate n f
  where
    M (Z :. _m :. n) xs = manifest e

    f :: Int -> e
    f i = xs V.! (i*n + j)

-- | Alias for Kronecker product.
infixl 7 ⊗
(⊗) :: SPL e -> SPL e -> SPL e
(⊗) = Kron

-- | Alias for matrix direct sum.
infixl 6 ⊕
(⊕) :: SPL e -> SPL e -> SPL e
(⊕) = DSum

-- | Alias for matrix product.
infixl 7 ×
(×) :: SPL e -> SPL e -> SPL e
(×) = Prod
