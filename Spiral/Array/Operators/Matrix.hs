{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.Matrix
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Matrix (
    row,
    rowS,

    col,
    colS,

    nrows,
    ncols,

    (<->),
    (<|>),
    block,

    mXv,
    (#>),
    mXvP,

    transpose,
    mXm,
    kronecker,
    directSum
  ) where

import Prelude hiding ((!!))

import Control.Monad (when)
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Array.Operators.Mapping
import Spiral.Array.Operators.Reduction
import Spiral.Exp
import Spiral.Monad
import Spiral.Program.Monad

-- | Extract a row from a matrix.
row :: IArray r DIM2 a
    => Matrix r a
    -> Int
    -> Vector D a
row a i = fromFunction (Z :. n) g
  where
    (Z :. _m :. n, f) = toFunction (delay a)

    g (Z :. j) = f (Z :. i :. j)

-- | Extract a symbolic row from a matrix.
rowS :: SArray r DIM2 a
     => Matrix r a
     -> Exp Int
     -> Vector DS a
rowS a i = fromSFunction (Z :. n) g
  where
    (Z :. _m :. n, f) = toSFunction (delayS a)

    g (Z :. j) = f (Z :. i :. j)

-- | Extract a column from a matrix.
col :: IArray r DIM2 a
    => Matrix r a
    -> Int
    -> Vector D a
col a j = fromFunction (Z :. m) g
  where
    (Z :. m :. _n, f) = toFunction (delay a)

    g (Z :. i) = f (Z :. i :. j)

-- | Extract a symbolic column from a matrix.
colS :: SArray r DIM2 a
     => Matrix r a
     -> Exp Int
     -> Vector DS a
colS a j = fromSFunction (Z :. m) g
  where
    (Z :. m :. _n, f) = toSFunction (delayS a)

    g (Z :. i) = f (Z :. i :. j)

-- | Number of rows
nrows :: IsArray r DIM2 a => Matrix r a -> Int
nrows a = m
  where
    Z :. m :. _n = extent a

-- | Number of columns
ncols :: IsArray r DIM2 a => Matrix r a -> Int
ncols a = n
  where
    Z :. _m :. n = extent a

-- | Vertical stacking of matrices
(<->) :: ( IArray r1 DIM2 a
         , IArray r2 DIM2 a
         )
      => Matrix r1 a
      -> Matrix r2 a
      -> Matrix D a
a <-> b
    | ncols b /= c = error "(<->): Incompatible matrix dimensions"
    | otherwise    = fromFunction (ix2 (r + nrows b) c) f
  where
    Z :. r :. c = extent a

    f (Z :. i :. j)
      | i < r     = a ! (i,j)
      | otherwise = b ! (i-r,j)

-- | Horizontal stacking of matrices
(<|>) :: ( IArray r1 DIM2 a
         , IArray r2 DIM2 a
         )
      => Matrix r1 a
      -> Matrix r2 a
      -> Matrix D a
a <|> b
    | nrows b /= r = error "(<|>): Incompatible matrix dimensions"
    | otherwise    = fromFunction (ix2 r (c + ncols b)) f
  where
    Z :. r :. c = extent a

    f (Z :. i :. j)
      | j < c     = a ! (i,j)
      | otherwise = b ! (i,j-c)

-- | Create a block matrix
block:: ( IArray r1 DIM2 a
        , IArray r2 DIM2 a
        , IArray r3 DIM2 a
        , IArray r4 DIM2 a
        )
     => Matrix r1 a
     -> Matrix r2 a
     -> Matrix r3 a
     -> Matrix r4 a
     -> Matrix D a
block tl tr bl br = (tl <|> tr) <-> (bl <|> br)

-- | Compute the matrix-vector product, @A x@.
mXv :: forall r1 r2 a .
       ( Num a
       , IArray r1 DIM2 a
       , IArray r2 DIM1 a
       )
    => Matrix r1 a -- ^ The matrix @A@
    -> Vector r2 a -- ^ The vector @x@
    -> Vector D  a
mXv a x
  | n' /= n   = errordoc $ text "mXv: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
  | otherwise = fromFunction (Z :. m) f
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    f :: DIM1 -> a
    f (Z :. i) = sum (map (t !) [0..n-1])
      where
        t :: Vector D a
        t = x .* row a i

-- | Compute the matrix-vector product, @A x@.
infixr 8 #>
(#>) :: forall r1 r2 a .
        ( Num a
        , IArray r1 DIM2 a
        , IArray r2 DIM1 a
        )
     => Matrix r1 a -- ^ The matrix @A@
     -> Vector r2 a -- ^ The vector @x@
     -> Vector D  a
(#>) = mXv

-- | Transpose a matrix
transpose :: forall e r . IArray r DIM2 e
          => Array r DIM2 e
          -> Array D DIM2 e
transpose a = D (ix2 n m) f
  where
    Z :. m :. n = extent a

    f (Z :. i :. j) = index a (Z :. j :. i)

-- | Compute the matrix-vector product, @y = A x@.
mXvP :: forall r1 r2 r3 a m .
        ( MonadSpiral m
        , Num (Exp a)
        , Typed a
        , IArray r1 DIM2 (Exp a)
        , SArray r2 DIM1 (Exp a)
        , MArray r3 DIM1 (Exp a)
        )
     => Matrix r1 (Exp a) -- ^ The matrix @A@
     -> Vector r2 (Exp a) -- ^ The vector @x@
     -> Vector r3 (Exp a) -- ^ The vector @y@
     -> P m ()
mXvP a x y = do
    when (n' /= n) $
      faildoc $ text "mXvP: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
    shouldUnroll n >>= go y
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    go :: Vector r3 (Exp a)
       -> Bool
       -> P m ()
    go y True =
        computeP y (fromSFunction (Z :. m) f)
      where
        f :: ExpShapeOf DIM1 -> Exp a
        f (Z :. i) = sum (map (t !!) [0..n-1])
          where
            -- XXX we know we are unrolling here, so we can coerce a to a
            -- symbolic array since we know we will only index it with integer
            -- constant indices.
            t = x ^* rowS (coerceSymbolic a) i

    go y _ = do
        a' <- cacheArray a
        forP 0 m $ \i -> do
            yi <- sumP $ x ^* rowS a' i
            write y (Z :. i) yi

-- | Kronecker product of two matrices.
kronecker :: forall r1 r2 a .
             ( Num a
             , IArray r1 DIM2 a
             , IArray r2 DIM2 a
             )
          => Matrix r1 a
          -> Matrix r2 a
          -> Matrix D  a
kronecker a b = fromFunction (ix2 (m*p) (n*q)) f
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j) = a ! ix2 (i `quot` p) (j `quot` q) *
                       b ! ix2 (i `rem` p)  (j `rem` q)

-- | Direct sum of two matrices.
directSum :: forall r1 r2 a .
             ( Num a
             , IArray r1 DIM2 a
             , IArray r2 DIM2 a
             )
          => Matrix r1 a
          -> Matrix r2 a
          -> Matrix D  a
directSum a b = fromFunction (ix2 (m+p) (n+q)) f
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j)
        | i < m  && j < n  = a ! ix2 i j
        | i >= m && j >= n = b ! ix2 (i-m) (j-n)
        | otherwise        = 0

-- | Product of two matrices.
mXm :: forall r1 r2 a .
       ( Num a
       , IArray r1 DIM2 a
       , IArray r2 DIM2 a
       )
    => Matrix r1 a
    -> Matrix r2 a
    -> Matrix D  a
mXm a b
    | p /= p'   = error "mxM: matrix dimensions don't match"
    | otherwise = fromFunction (ix2 m n) f
  where
    Z :. m  :. p' = extent a
    Z :. p :. n   = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j) = sum' $ row a i .* col b j

    sum' :: (Num a, IArray r DIM1 a) => Vector r a -> a
    sum' v = sum [v ! i | i <- [0..n-1]]
      where
          Z :. n = extent v
