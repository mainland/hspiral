{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Complex
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Operators.IndexSpace
import Spiral.Array.Operators.Permute
import Spiral.Array.Program
import Spiral.Array.Repr.Complex
import Spiral.Array.Repr.Compute
import Spiral.Array.Repr.Slice
import Spiral.Array.Repr.Transform
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Trace

infix 4 .<-.
(.<-.) :: forall r1 r2 m a . (MonadP m, MArray r1 DIM1 a, Compute r2 DIM1 a)
       => Vector r1 a
       -> m (Vector r2 a)
       -> m ()
y .<-. k = do
    x <- k
    computeP y x

runSPL :: forall m a .
          ( MonadP m
          , Typed a
          , Num (Exp a)
          )
       => SPL (Exp a)
       -> Vector T (Exp a)
       -> m (Vector T (Exp a))
runSPL e@E{} x = do
    comment $ ppr e
    t <- gather x
    computeTransform e $ \y ->
      mvP (toMatrix e) t y

runSPL I{} x =
    return x

runSPL (J n) x =
    return $ permute (JP n) x

runSPL (L mn n) x =
    return $ permute (LP mn n) x

runSPL e@(Diag v) x = do
    comment $ ppr e
    t <- gather x
    computeTransform e $ \y ->
      shouldUnroll n >>= go t y
  where
    n :: Int
    n = V.length v

    go :: forall r1 r2 m . (MonadP m, SArray r1 DIM1 (Exp a), MArray r2 DIM1 (Exp a))
       => Array r1 DIM1 (Exp a)
       -> Array r2 DIM1 (Exp a)
       -> Bool
       -> m ()
    go x y True =
        forP 0 n $ \ei@(ConstE (IntC i)) ->
            write y (Z :. ei) (v V.! i * indexS x (Z :. ei))

    go x y _ = do
        d <- cacheArray (fromFunction (ix1 n) f)
        forShapeP (ix1 n) $ \eix ->
            write y eix (indexS d eix * indexS x eix)
      where
        f (Z :. i) = v V.! i

runSPL e@(Kron (I m) a) x | n' == n = do
    comment $ ppr e
    t <- gather x
    computeTransform e $ \y ->
      forP 0 m $ \i ->
        slice y (i*fromIntegral n) 1 n .<-. runSPL a (fromGather (slice t (i*fromIntegral n) 1 n))
  where
    Z :. n :. n' = splExtent a

runSPL e@(Kron a (I n)) x | m' == m = do
    comment $ ppr e
    t <- gather x
    computeTransform e $ \y ->
      forP 0 n $ \i ->
        slice y i n m .<-. runSPL a (fromGather (slice t i n m))
  where
   Z :. m :. m' = splExtent a

runSPL e@(DSum a b) x | m' == m && n' == n = do
    comment $ ppr e
    t <- gather x
    computeTransform e $ \y -> do
      slice y 0 1 m                .<-. runSPL a  (fromGather (slice t 0 1 m))
      slice y (fromIntegral m) 1 n .<-. runSPL b  (fromGather (slice t (fromIntegral m) 1 n))
  where
    Z :. m :. m' = splExtent a
    Z :. n :. n' = splExtent b

runSPL e@(Prod a b) x | n' == n = do
    comment $ ppr e
    t <- runSPL b x
    runSPL a t
  where
    Z :. _m :.  n = splExtent a
    Z :. n' :. _p = splExtent b

runSPL (Re a) x = do
    t <- gather x
    computeTransform (Re a) $ \y -> do
      let t' :: Vector (CMPLX DS) (Exp (Complex a)) = toComplexArray t
      toComplexArray y .<-. runSPL a (fromGather t')

runSPL a@F2{} x = do
    comment $ ppr a
    t <- gather x
    computeTransform a $ \y ->
      mvP (toMatrix a) t y

runSPL a x = do
    traceCg $ text "Falling back to default compilation path:" </> ppr a
    comment $ ppr a
    t <- gather x
    computeTransform a $ \y ->
      mvP (toMatrix a) t y

computeTransform :: Monad m
                 => SPL a
                 -> (forall r m . (MArray r DIM1 a, MonadP m) => Array r DIM1 a -> m ())
                 -> m (Array T DIM1 a)
computeTransform a k =
    return $ fromScatter $ fromCompute (ix1 m) k
  where
    Z :. m :. _n = splExtent a
