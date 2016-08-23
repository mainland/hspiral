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
    module Spiral.Array,
    module Spiral.Shape,

    Array(..),

    SPL,
    spl,

    (⊗),
    (×),
    (⊕),
  ) where

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Shape
import Spiral.Util.Pretty

-- | Type tag for an SPL matrix.
data SPL

-- | Embed any 'Array' as an SPL term.
spl :: (IsArray r sh e, Pretty (Array r sh e))
    => Array r sh e
    -> Array SPL sh e
spl = E

instance Num e => IsArray SPL sh e where
    -- | A "delayed" matrix, i.e., functional representation.
    data Array SPL sh e where
        -- An "embedded" array with an unknown representation.
        E :: (IsArray r sh e, Pretty (Array r sh e))
          => Array r sh e
          -> Array SPL sh e

        -- The $n \times n$ identity matrix.
        I :: Int -> Matrix SPL e

        -- The $L^{rs}_s$ $rs \times rs$ stride permutation matrix with stride $s$.
        L :: Int -> Int -> Matrix SPL e

        -- Binary operation on 2-d matrices
        B :: MatrixBinop -> Matrix SPL e -> Matrix SPL e -> Matrix SPL e

    extent (E a)     = extent a
    extent (I n)     = ix2 n n
    extent (L rs _s) = ix2 rs rs

    extent (B op a b) = go op
      where
        Z :. m :. n = extent a
        Z :. p :. q = extent b

        go :: MatrixBinop -> sh
        go K  = ix2 (m*p) (n*q)
        go DS = ix2 (m+p) (n+q)
        go P  = ix2 m q

    index (E a) i = index a i
    index (I _sh) i =
        case listOfShape i of
          i:js | all (== i) js -> 1
          _ -> 0

    index (L rs s) (Z :. i :. j)
        | j == i*s `mod` rs + i*s `div` rs = 1
        | otherwise                        = 0

    index (B op a b) ix =
        go op ix
      where
        Z :. m :. n = extent a
        Z :. p :. q = extent b

        go :: MatrixBinop -> sh -> e
        go K (Z :. i :. j) =
            a ! ix2 (i `quot` p) (j `quot` q) *
            b ! ix2 (i `rem` p) (j `rem` q)

        go DS (Z :. i :. j)
            | i < m  && j < n  = a ! ix2 i j
            | i >= m && j >= n = b ! ix2 (i-m) (j-n)
            | otherwise        = 0

        go P (Z :. i :. j) =
            sum [a ! ix2 i k * b ! ix2 k j | k <- [0..n-1]]

    manifest (E a) = manifest a

    manifest (B op a b) =
        go op
      where
        a' = manifest a
        b' = manifest b

        (Z :. m :. n) = extent a
        (Z :. p :. q) = extent b

        go :: MatrixBinop -> Array M sh e
        go K = M (ix2 (m*p) (n*q)) $ V.generate (m*p*n*q) f
          where
            f :: Int -> e
            f k = a' ! ix2 (i `quot` p) (j `quot` q) *
                  b' ! ix2 (i `rem` p) (j `rem` q)
              where
                (i, j) = k `quotRem` (n*q)

        go DS = M (ix2 (m+p) (n+q)) $ V.generate ((m+p)*(n+q)) f
          where
            f :: Int -> e
            f k | i < m  && j < n  = a' ! ix2 i j
                | i >= m && j >= n = b' ! ix2 (i-m) (j-n)
                | otherwise        = 0
              where
                (i, j) = k `quotRem` (n+q)

        go P = M (ix2 m p) $ V.generate (m*p) f
          where
            f :: Int -> e
            f k = V.sum $ V.zipWith (*) (row a' i) (col b' j)
              where
                (i, j) = k `quotRem` p

    manifest a = M sh $ V.fromList [a ! fromIndex sh i | i <- [0..size sh-1]]
      where
        sh :: sh
        sh = extent a

instance (Num e, Pretty e) => Pretty (Matrix SPL e) where
    pprPrec p (E a)      = pprPrec p a
    pprPrec _ (I n)      = text "I_" <> ppr n
    pprPrec _ (L rs s)   = text "L^" <> ppr rs <> char '_' <> ppr s
    pprPrec p (B op a b) = infixop p op a b

data MatrixBinop = K
                 | DS
                 | P
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
    fixity K  = infixl_ 7
    fixity DS = infixl_ 6
    fixity P  = infixl_ 7

instance Pretty MatrixBinop where
    ppr K  = char '⊗'
    ppr DS = char '⊕'
    ppr P  = char '×'

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
(⊗) :: Matrix SPL e -> Matrix SPL e -> Matrix SPL e
(⊗) = B K

-- | Alias for matrix direct sum.
infixl 6 ⊕
(⊕) :: Matrix SPL e -> Matrix SPL e -> Matrix SPL e
(⊕) = B DS

-- | Alias for matrix product.
infixl 7 ×
(×) :: Matrix SPL e -> Matrix SPL e -> Matrix SPL e
(×) = B P
