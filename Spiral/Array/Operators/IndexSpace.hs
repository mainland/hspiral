{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.IndexSpace
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.IndexSpace (
    row,
    rowS,

    mv,
    mvP
  ) where

import Prelude hiding ((!!))

import Control.Monad (when)
import Text.PrettyPrint.Mainland
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

-- | Compute the matrix-vector product, @A x@.
mv :: forall r1 r2 a .
      ( Num a
      , IArray r1 DIM2 a
      , IArray r2 DIM1 a
      )
   => Matrix r1 a -- ^ The matrix @A@
   -> Vector r2 a -- ^ The vector @x@
   -> Vector D  a
mv a x
  | n' /= n   = errordoc $ text "mv: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
  | otherwise = fromFunction (Z :. m) f
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    f :: DIM1 -> a
    f (Z :. i) = sum (map (t !) [0..n-1])
      where
        t :: Vector D a
        t = x .* row a i

-- | Compute the matrix-vector product, @y = A x@.
mvP :: forall r1 r2 r3 a m .
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
mvP a x y = do
    when (n' /= n) $
      faildoc $ text "mvP: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
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
