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
    matrix,
    fromLists,
    fromFunction,

    diag,
    circ,
    toep,
    permute,
    backpermute,

    extent,

    toMatrix,
    col,
    row,

    (⊗),
    (×),
    (⊕),

    Z(..),
    (:.)(..),

    DIM0,
    DIM1,
    DIM2,
    ix1,
    ix2
  ) where

import Data.Complex
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import qualified Spiral.Array as A
import Spiral.Array (IArray,
                     M,
                     Matrix,
                     manifest)
import qualified Spiral.Array.Operators.Matrix as A
import Spiral.Array.Repr.Complex
import Spiral.Array.Shape
import Spiral.Exp
import Spiral.Permutation
import Spiral.RootOfUnity
import Spiral.Util.Pretty

-- | Wrap an array to ensure it is unconditionally Show-able.
newtype ShowArray r sh e = ShowArray { unShowArray :: Array r sh e }

instance (Show sh, Show e, IArray r sh e) => Show (ShowArray r sh e) where
    show arr = show (manifest (unShowArray arr))

-- | An SPL transform. The type index is the type of the scalar values in the
-- transformed vector.
data SPL a where
    -- | An "embedded" array with an unknown representation.
    E :: IArray r DIM2 e => ShowArray r DIM2 e -> SPL e

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

-- | Embed any 'Matrix' as an SPL term.
matrix :: IArray r DIM2 e
       => Array r DIM2 e
       -> SPL e
matrix = E . ShowArray

-- | Embed a matrix created from a list of lists of values.
fromLists :: [[e]] -> SPL e
fromLists = matrix . A.fromLists

-- | Embed a matrix created from a function mapping indices to elements.
fromFunction :: DIM2 -> (DIM2 -> e) -> SPL e
fromFunction sh = matrix . A.fromFunction sh

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
extent :: SPL a -> DIM2
extent (E a)     = A.extent (unShowArray a)
extent (I n)     = ix2 n n
extent (Pi p)    = ix2 (dim p) (dim p)
extent (Rot _)   = ix2 2 2

extent (Diag xs) = ix2 n n
  where
    n = length xs

extent (KDiag n _) = ix2 n n

extent (Kron a b) = ix2 (m*p) (n*q)
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

extent (DSum a b) = ix2 (m+p) (n+q)
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

extent (Prod a b) = ix2 m q
  where
    Z :. m  :. _n = extent a
    Z :. _p :. q  = extent b

extent (Circ xs) = ix2 n n
  where
    n = length xs

extent (Toep xs) = ix2 n n
  where
    n = (length xs + 1) `quot` 2

extent (Re a) = ix2 (2*m) (2*n)
  where
    Z :. m  :. n = extent a

extent F2 = ix2 2 2

extent (DFT n)  = ix2 n n
extent (DFT' n) = ix2 n n
extent (F n _)  = ix2 n n
extent (F' n _) = ix2 n n

-- | Convert an SPL transform to an explicit matrix.
toMatrix :: forall e . Num e => SPL e -> Matrix M e
toMatrix (E a) =
    manifest (unShowArray a)

toMatrix (Diag xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) | i == j    = xs V.! i
                    | otherwise = 0

toMatrix (KDiag n e) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = e
                    | otherwise = 0

toMatrix (Kron a b) =
    A.manifest $ A.kronecker (toMatrix a) (toMatrix b)

toMatrix (DSum a b) =
    A.manifest $ A.directSum (toMatrix a) (toMatrix b)

toMatrix (Prod a b) =
    A.manifest $ A.mXm (toMatrix a) (toMatrix b)

toMatrix (Circ xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) = xs V.! ((i-j) `mod` n)

toMatrix (Toep xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = (length xs + 1) `quot` 2

    f (Z :. i :. j) = xs V.! (i-j+n-1)

toMatrix (I n) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0

toMatrix (Rot alpha) =
    A.matrix [[cos alpha, -(sin alpha)],
              [sin alpha, cos alpha]]

toMatrix (Pi p) =
    manifest $ A.fromFunction (ix2 (dim p) (dim p)) f
  where
    f (Z :. i :. j) | g j == i  = 1
                    | otherwise = 0

    g = toIdxMapping p

toMatrix (Re a) = manifest (RE (toMatrix a))

toMatrix F2 =
    A.matrix [[1,  1],
              [1, -1]]

toMatrix (DFT n)  = toMatrix (F n (omega n))
toMatrix (DFT' n) = toMatrix (F' n (omega n))

toMatrix (F n w) = manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) = w ^ (i*j)

toMatrix (F' n w) = toMatrix (KDiag n (1/fromIntegral n) × F n (1/w))

pprArgs :: Pretty a => [a] -> Doc
pprArgs = parens . commasep . map ppr

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec p (E a)       = pprPrec p (manifest (unShowArray a))
    pprPrec _ (I n)       = text "I_" <> ppr n
    pprPrec _ (Pi p)      = ppr p
    pprPrec _ (Rot alpha) = text "R_" <> ppr alpha
    pprPrec _ (Diag xs)   = text "diag" <> pprArgs (V.toList xs)
    pprPrec _ (KDiag n e) = text "kdiag" <> parens (commasep [ppr n, ppr e])
    pprPrec p (Kron a b)  = infixop p KOp a b
    pprPrec p (DSum a b)  = infixop p DSOp a b
    pprPrec p (Prod a b)  = infixop p POp a b
    pprPrec _ (Circ xs)   = text "circ" <> pprArgs (V.toList xs)
    pprPrec _ (Toep xs)   = text "toep" <> pprArgs (V.toList xs)
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
col e j = V.generate m f
  where
    M (Z :. m :. n) xs = manifest e

    f :: Int -> e
    f i = xs V.! (i*n + j)

-- | Alias for Kronecker product.
infixl 7 ⊗
(⊗) :: SPL e -> SPL e -> SPL e
I m ⊗ (Kron (I n) y) = I (m+n) ⊗ y
I 1 ⊗ y              = y
x   ⊗ I 1            = x
x   ⊗ y              = Kron x y

-- | Alias for matrix direct sum.
infixl 6 ⊕
(⊕) :: SPL e -> SPL e -> SPL e
I m     ⊕ I n     = I (n + m)
Diag xs ⊕ Diag ys = Diag (xs <> ys)
x       ⊕ y       = DSum x y

-- | Alias for matrix product.
infixl 7 ×
(×) :: SPL e -> SPL e -> SPL e
(×) = Prod
