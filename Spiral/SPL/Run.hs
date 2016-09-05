{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Spiral.SPL.Run
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.SPL.Run (
    runSPL
  ) where

import Prelude hiding ((!!), read)

import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Operators.IndexSpace
import Spiral.Array.Program
import Spiral.Array.Repr.Slice
import Spiral.Array.Repr.Virtual
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Trace

infix 4 .<-.
(.<-.) :: a -> (a -> b) -> b
a .<-. f = f a

runSPL :: ( MonadP m
          , Typed a
          , Num (Exp a)
          , SArray r1 DIM1 (Exp a)
          , MArray r2 DIM1 (Exp a)
          )
       => SPL (Exp a)
       -> Vector r1 (Exp a)
       -> Vector r2 (Exp a)
       -> m ()
runSPL e@E{} x y = do
    comment $ ppr e
    mvP (toMatrix e) x y

runSPL I{} x y =
    computeP y x

runSPL J{} x y =
    computeP y (backpermute f x)
  where
    n :: Int
    Z :. n = extent x

    f :: forall a . Integral a => a -> a
    f i = fromIntegral n - 1 - i

runSPL (L mn n) x y =
    computeP y $ backpermute (lperm (fromIntegral mn) (fromIntegral n)) x

runSPL e@(Diag v) x y = do
    comment $ ppr e
    shouldUnroll n >>= go
  where
    n :: Int
    n = V.length v

    go True =
        forP 0 n $ \ei@(ConstE (IntC i)) ->
            write y (Z :. ei) (v V.! i * indexS x (Z :. ei))

    go _ = do
        d <- cacheArray (fromFunction (ix1 n) f)
        forShapeP (ix1 n) $ \eix ->
            write y eix (indexS d eix * indexS x eix)
      where
        f (Z :. i) = v V.! i

runSPL e@(Kron (I m) a) x y | n' == n = do
    comment $ ppr e
    forP 0 m $ \i ->
      slice y (i*fromIntegral n) 1 n .<-. runSPL a (slice x (i*fromIntegral n) 1 n)
  where
    Z :. n :. n' = splExtent a

runSPL e@(Kron a (I n)) x y | m' == m = do
    comment $ ppr e
    forP 0 n $ \i ->
      slice y i n m .<-. runSPL a (slice x i n m)
  where
   Z :. m :. m' = splExtent a

runSPL e@(DSum a b) x y | m' == m && n' == n = do
    comment $ ppr e
    slice y 0 1 m                .<-. runSPL a  (slice x 0 1 m)
    slice y (fromIntegral m) 1 n .<-. runSPL b  (slice x (fromIntegral m) 1 n)
  where
    Z :. m :. m' = splExtent a
    Z :. n :. n' = splExtent b

runSPL e@(Prod a b) x y | n' == n = do
    comment $ ppr e
    shouldUnroll n >>= go
  where
    Z :. _m :.  n = splExtent a
    Z :. n' :. _p = splExtent b

    -- XXX Again, we know we are unrolling, so we can coerce the result of
    -- freezing our temporary array to symbolic form.
    go True = do
        t <- replicateV (ix1 n) 0
        runSPL b x t
        t' <- freezeV t
        runSPL a (coerceSymbolic t') y

    go _ = do
        t <- newArray (ix1 n)
        runSPL b x t
        runSPL a t y

runSPL e@F2{} x y = do
    comment $ ppr e
    mvP (toMatrix e) x y

runSPL a x y = do
    traceCg $ text "Falling back to default compilation path:" </> ppr a
    mvP (toMatrix a) x y
