{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.IndexSpace
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.IndexSpace (
    backpermute,

    row,

    mvP
  ) where

import Prelude hiding ((!!))

import Control.Monad (when)
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Operators.Mapping
import Spiral.Array.Operators.Reduction
import Spiral.Array.Program
import Spiral.Exp

backpermute :: SArray r DIM1 a
            => (forall a . Integral a => a -> a)
            -> Vector r a
            -> Vector DS a
backpermute f x = fromSFunction (extent x) (\(Z :. i) -> indexS x (Z :. f i))

row :: SArray r DIM2 a
    => Matrix r a
    -> Exp Int
    -> Vector DS a
row a i = fromSFunction (Z :. n) g
  where
    (Z :. _m :. n, f) = toSFunction (sdelay a)

    g (Z :. j) = f (Z :. i :. j)

-- | Compute the matrix-vector product, @y = A x@.
mvP :: forall r1 r2 r3 a m .
       ( MonadP m
       , Num (Exp a)
       , Assign (Exp a) (Exp a)
       , IArray r1 DIM2 (Exp a)
       , SArray r2 DIM1 (Exp a)
       , MArray r3 DIM1 (Exp a)
       )
    => Matrix r1 (Exp a) -- ^ The matrix @A@
    -> Vector r2 (Exp a) -- ^ The vector @x@
    -> Vector r3 (Exp a) -- ^ The vector @y@
    -> m ()
mvP a x y = do
    when (n' /= n) $
      faildoc $ text "mvP: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
    shouldUnroll n >>= go y
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    go :: forall m . (MonadP m, MArray r3 DIM1 (Exp a))
       => Vector r3 (Exp a)
       -> Bool
       -> m ()
    go y True =
        y .:=. fromSFunction (Z :. m) f
      where
        f (Z :. i) = sum (map (t !!) [0..n-1])
          where
            -- XXX we know we are unrolling here, so we can coerce a to a
            -- symbolic array since we know we will only index it with integer
            -- constant indices.
            t = x .* row (coerceSymbolic a) i

    go y _ = do
        a' <- cacheMatrix a
        forP 0 m $ \i -> do
            yi <- sumP $ x .* row a' i
            write y (Z :. i) yi
