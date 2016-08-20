{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  SPL.Syntax
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Syntax (
    Ix,
    SPL(..),

    extent,

    toDelayed,
    toMatrix,

    matrix,
    (!),
    row,
    col,

    (⊗),
    (×),
    (⊕),

    matrixOf,

    pprMatrix
  ) where

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import SPL.Pretty

-- | A matrix index
type Ix = (Int, Int)

-- | Abstract syntax for SPL
data SPL e -- | A matrix whose entries are manifest
           = M Ix (V.Vector e)
           -- | A "delayed" matrix, i.e., functional representation
           | D Ix (Ix -> e)

           -- | The $n \times n$ identity matrix,
           | I Int

           -- | The $L^{rs}_s$ $rs \times rs$ stride permutation matrix with
           -- stride $s$.
           | L Int Int

           -- Binary matrix operation
           | BinopM Binop (SPL e) (SPL e)

-- | Binary operators
data Binop = KroneckerOp
           | DSumOp
           | ProductOp

instance Pretty Binop where
    ppr KroneckerOp = char '⊗'
    ppr DSumOp      = char '⊕'
    ppr ProductOp   = char '×'

instance HasFixity Binop where
    fixity KroneckerOp = infixl_ 7
    fixity DSumOp      = infixl_ 6
    fixity ProductOp   = infixl_ 7

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec _ e@M{} = pprMatrix e
    pprPrec _ e@D{} = pprMatrix e

    pprPrec _ (I n) =
        text "I_" <> ppr n

    pprPrec _ (L rs s) =
        text "L^" <> ppr rs <> char '_' <> ppr s

    pprPrec p (BinopM op e1 e2) =
        infixop p op e1 e2

-- | Specify a matrix as a list of lists.
matrix :: [[e]] -> SPL e
matrix [] =
    M (0, 0) V.empty

matrix (r:rs) | all ((== n) . length) rs =
    M (m, n) (V.fromList (concat (r:rs)))
  where
    m = 1 + length rs
    n = length r

matrix _ = error "matrix: rows have differing lengths"

infixl 7 ⊗
-- | Kronecker product
(⊗) :: SPL e -> SPL e -> SPL e
(⊗) = BinopM KroneckerOp

infixl 7 ×
-- | Matrix product
(×) :: SPL e -> SPL e -> SPL e
(×) = BinopM ProductOp

infixl 6 ⊕
-- | Matrix direct sum
(⊕) :: SPL e -> SPL e -> SPL e
(⊕) = BinopM DSumOp

-- | Calculate the extent (shape) of the matrix represented by an SPL
-- expression.
extent :: SPL e -> Ix
extent (M sh _) = sh
extent (D sh _) = sh
extent (I n)    = (n, n)
extent (L rs _) = (rs, rs)

extent (BinopM op a b) = go op
  where
    (m, n) = extent a
    (p, q) = extent b

    go :: Binop -> Ix
    go KroneckerOp = (m*p, n*q)
    go DSumOp      = (m+p, n+q)
    go ProductOp
      | n == p     = (m, q)
      | otherwise  = error "(×): mismatched matrix dimensions"

-- | Convert an SPL expression to a delayed matrix
toDelayed :: forall e . Num e => SPL e -> SPL e
toDelayed (M sh@(_m, n) xs) = D sh f
  where
    f (i, j) = xs V.! (i*n + j)

toDelayed e@D{} = e

toDelayed (I n) = D (n, n) f
  where
    f :: Ix -> e
    f (i, j) | j == i    = 1
             | otherwise = 0

toDelayed (L rs s) = D (rs, rs) f
  where
    f :: Ix -> e
    f (i, j) | j == i*s `mod` rs + i*s `div` rs = 1
             | otherwise                        = 0

toDelayed (BinopM KroneckerOp a b) = D (m*p, n*q) h
  where
    D (m, n) f = toDelayed a
    D (p, q) g = toDelayed b

    h (i, j) = f (i `quot` p, j `quot` q) * g (i `rem` p, j `rem` q)

toDelayed (BinopM DSumOp a b) = D (m+p, n+q) h
  where
    D (m, n) f = toDelayed a
    D (p, q) g = toDelayed b

    h (i, j)
      | i < m  && j < n  = f (i, j)
      | i >= m && j >= n = g (i-m, j-n)
      | otherwise      = 0

toDelayed (BinopM ProductOp a b) = D (m, p) h
  where
    D (m, _n) f = toDelayed a
    D (n, p)  g = toDelayed b

    h (i, j) = sum [f (i, k) * g (k, j) | k <- [0..n-1]]

-- | Extract a single element from a matrix
(!) :: Num e => SPL e -> Ix -> e
M (_m, n) xs ! (i, j) = xs V.! (i*n + j)
e            ! ix     = toMatrix e ! ix

-- | Extract a row of a matrix
row :: Num e => SPL e -> Int -> V.Vector e
row e i = V.slice (i*n) n xs
  where
    M (_m, n) xs = toMatrix e

-- | Extract a column of a matrix
col :: forall e . Num e => SPL e -> Int -> V.Vector e
col e j = V.generate n f
  where
    M (_m, n) xs = toMatrix e

    f :: Int -> e
    f i = xs V.! (i*n + j)

-- | Convert an SPL expression to a manifest matrix.
toMatrix :: forall e . Num e => SPL e -> SPL e
toMatrix e@M{} =
    e

toMatrix (D (m, n) f) =
    M (m, n) $ V.fromList [f (i, j) | i <- [0..m-1], j <- [0..n-1]]

toMatrix (I n) = M (n, n) $ V.generate (n*n) f
  where
    f :: Int -> e
    f k | j == i    = 1
        | otherwise = 0
      where
        (i, j) = k `quotRem` n

toMatrix (L rs s) = M (rs, rs) $ V.generate (rs*rs) f
  where
    f :: Int -> e
    f k | j == i*s `mod` rs + i*s `div` rs = 1
        | otherwise                        = 0
      where
        (i, j) = k `quotRem` rs

toMatrix (BinopM KroneckerOp a b) = M (m*p, n*q) $ V.generate (m*p*n*q) h
  where
    as@(M (m, n) _) = toMatrix a
    bs@(M (p, q) _) = toMatrix b

    h k = as ! (i `quot` p, j `quot` q) * bs ! (i `rem` p, j `rem` q)
      where
        (i, j) = k `quotRem` (n*q)

toMatrix (BinopM DSumOp a b) = M (m+p, n+q) $ V.generate ((m+p)*(n+q)) h
  where
    as@(M (m, n) _) = toMatrix a
    bs@(M (p, q) _) = toMatrix b

    h k | i < m  && j < n  = as ! (i, j)
        | i >= m && j >= n = bs ! (i-m, j-n)
        | otherwise      = 0
      where
        (i, j) = k `quotRem` (n+q)

toMatrix (BinopM ProductOp a b) = M (m, p) $ V.generate (m*p) h
  where
    as@(M (m,   _n1) _) = toMatrix a
    bs@(M (_n2,   p) _) = toMatrix b

    h k = V.sum $ V.zipWith (*) (row as i) (col bs j)
      where
        (i, j) = k `quotRem` p

-- | Convert an SPL expression to a matrix in the form of a list of lists
matrixOf :: Num e => SPL e -> [[e]]
matrixOf e = [[a ! (i, j) | i <- [0..m-1]] | j <- [0..n-1]]
  where
    a@(M (m, n) _) = toMatrix e

-- | Pretty-print an 'SPL e' expression as a manifest matrix
pprMatrix :: (Num e, Pretty e) => SPL e -> Doc
pprMatrix e =
    brackets $ align $
    folddoc (\d1 d2 -> d1 <> comma </> d2) $
    map ppr $
    matrixOf e
