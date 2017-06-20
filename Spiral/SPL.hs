{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.SPL
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.SPL (
    module Spiral.Permutation,

    SPL(..),
    spl,
    diag,
    circ,
    toep,
    permute,
    backpermute,

    splExtent,

    toMatrix,

    (⊗),
    (×),
    (⊕),
  ) where

import Data.Complex
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Array.Repr.Complex
import Spiral.Exp
import Spiral.Permutation
import Spiral.RootOfUnity
import Spiral.Util.Pretty

-- | An SPL transform. The type index is the type of the scalar values in the
-- transformed vector.
data SPL a where
    -- | An "embedded" array with an unknown representation.
    E :: (IArray r DIM2 e, Show (Array r DIM2 e))
      => Array r DIM2 e
      -> SPL e

    -- | The $n \times n$ identity matrix.
    I :: Num e => Int -> SPL e

    -- | A permutation
    Pi :: Permutation -> SPL e

    -- | The rotation matrix
    Rot :: Floating a => a -> SPL a

    -- | A diagonal matrix
    Diag :: V.Vector e -> SPL e

    -- | The $n \times n$ diagonal matrix with constant diagonal elements.
    KDiag :: Int -> e -> SPL e

    -- | Kronecker product
    Kron :: SPL e -> SPL e -> SPL e

    -- | Direct sum
    DSum :: SPL e -> SPL e -> SPL e

    -- | Matrix product
    Prod :: SPL e -> SPL e -> SPL e

    -- | Circulant matrix given first column
    Circ :: V.Vector e -> SPL e

    -- | Toeplitz matrix
    Toep :: V.Vector e -> SPL e

    -- | Make a complex transform into a real transform.
    Re :: RealFloatConst a => SPL (Exp (Complex a)) -> SPL (Exp a)

    -- | The 2x2 DFT
    F2 :: SPL e

    -- | The nxn DFT matrix
    DFT :: RootOfUnity a => Int -> SPL a

    -- | The nxn Inverse DFT matrix
    DFT' :: RootOfUnity a => Int -> SPL a

    -- | The nxn "rotated" DFT matrix
    F :: RootOfUnity a => Int -> a -> SPL a

    -- | The nxn "rotated" inverse DFT matrix
    F' :: RootOfUnity a => Int -> a -> SPL a

deriving instance Show e => Show (SPL e)
deriving instance Typeable e => Typeable (SPL e)

-- | Embed any 'Array' as an SPL term.
spl :: (IArray r DIM2 e, Show (Array r DIM2 e))
    => Array r DIM2 e
    -> SPL e
spl = E

-- | Create a diagonal matrix
diag :: [a] -> SPL a
diag = Diag . V.fromList

-- | Create a circulant matrix from the first column
circ :: [a] -> SPL a
circ = Circ . V.fromList

-- | Create a Toeplitz matrix
toep :: [a] -> SPL a
toep = Toep . V.fromList

-- | Permute (scatter).
permute :: Permutation -> SPL e
permute = Pi

-- | Backpermute (gather).
backpermute :: Permutation -> SPL e
backpermute = Pi . invert

-- | Return the extent of an SPL transform.
splExtent :: SPL a -> DIM2
splExtent (E a)     = extent a
splExtent (I n)     = ix2 n n
splExtent (Pi p)    = ix2 (dim p) (dim p)
splExtent (Rot _)   = ix2 2 2

splExtent (Diag xs) = ix2 n n
  where
    n = length xs

splExtent (KDiag n _) = ix2 n n

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

splExtent (Circ xs) = ix2 n n
  where
    n = length xs

splExtent (Toep xs) = ix2 n n
  where
    n = (length xs + 1) `quot` 2

splExtent (Re a) = ix2 (2*m) (2*n)
  where
    Z :. m  :. n = splExtent a

splExtent F2 = ix2 2 2

splExtent (DFT n)  = ix2 n n
splExtent (DFT' n) = ix2 n n
splExtent (F n _)  = ix2 n n
splExtent (F' n _) = ix2 n n

-- | Convert an SPL transform to an explicit matrix.
toMatrix :: forall e . Num e => SPL e -> Matrix M e
toMatrix (E a) =
    manifest a

toMatrix (Diag xs) =
    manifest $ fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) | i == j    = xs V.! i
                    | otherwise = 0

toMatrix (KDiag n e) =
    manifest $ fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = e
                    | otherwise = 0

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
    M (ix2 m n) $ V.generate (m*n) f
  where
    Z :. m  :. _p' = extent a'
    Z :. _p :. n   = extent b'

    a' = toMatrix a
    b' = toMatrix b

    f :: Int -> e
    f k = V.sum $ V.zipWith (*) (row a' i) (col b' j)
      where
        (i, j) = k `quotRem` n

toMatrix (Circ xs) =
    manifest $ fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) = xs V.! ((i-j) `mod` n)

toMatrix (Toep xs) =
    manifest $ fromFunction (ix2 n n) f
  where
    n = (length xs + 1) `quot` 2

    f (Z :. i :. j) = xs V.! (i-j+n-1)

toMatrix (I n) =
    manifest $ fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0

toMatrix (Rot alpha) =
    matrix [[cos alpha, -(sin alpha)],
            [sin alpha, cos alpha]]

toMatrix (Pi p) =
    manifest $ fromFunction (ix2 (dim p) (dim p)) f
  where
    f (Z :. i :. j) | g j == i  = 1
                    | otherwise = 0

    g = toIdxMapping p

toMatrix (Re a) = manifest (RE (toMatrix a))

toMatrix F2 =
    matrix [[1,  1],
            [1, -1]]

toMatrix (DFT n)  = toMatrix (F n (omega n))
toMatrix (DFT' n) = toMatrix (F' n (omega n))

toMatrix (F n w) = manifest $ fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) = w ^ (i*j)

toMatrix (F' n w) = toMatrix (KDiag n (1/fromIntegral n) × F n (1/w))

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec p (E a)       = pprPrec p (manifest a)
    pprPrec _ (I n)       = text "I_" <> ppr n
    pprPrec _ (Pi p)      = ppr p
    pprPrec _ (Rot alpha) = text "R_" <> ppr alpha
    pprPrec _ (Diag xs)   = text "diag" <> parens (commasep (map ppr (V.toList xs)))
    pprPrec _ (KDiag n e) = text "kdiag" <> parens (commasep [ppr n, ppr e])
    pprPrec p (Kron a b)  = infixop p KOp a b
    pprPrec p (DSum a b)  = infixop p DSOp a b
    pprPrec p (Prod a b)  = infixop p POp a b
    pprPrec _ (Circ xs)   = text "circ" <> parens (commasep (map ppr (V.toList xs)))
    pprPrec _ (Toep xs)   = text "toep" <> parens (commasep (map ppr (V.toList xs)))
    pprPrec _ (Re a)      = text "Re" <> parens (ppr a)
    pprPrec _ F2          = text "F_2"
    pprPrec _ (DFT n)     = text "DFT_" <> ppr n
    pprPrec _ (DFT' n)    = text "DFT'_" <> ppr n
    pprPrec _ (F n w)     = text "F_" <> ppr n <> parens (ppr w)
    pprPrec _ (F' n w)    = text "F'_" <> ppr n <> parens (ppr w)

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
row :: Matrix M e -> Int -> V.Vector e
row e i = V.slice (i*n) n xs
  where
    M (Z :. _m :. n) xs = manifest e

-- | Extract a column of a matrix
col :: forall e . Matrix M e -> Int -> V.Vector e
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
I m     ⊕ I n     = I (n + m)
I n     ⊕ Diag xs = Diag (V.replicate n 1 <> xs)
Diag xs ⊕ I n     = Diag (xs <> V.replicate n 1)
Diag xs ⊕ Diag ys = Diag (xs <> ys)
x       ⊕ y       = DSum x y

-- | Alias for matrix product.
infixl 7 ×
(×) :: SPL e -> SPL e -> SPL e
(×) = Prod
