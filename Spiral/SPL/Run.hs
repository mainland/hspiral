{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Spiral.SPL.Run
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.SPL.Run (
    toProgram,
    runSPL
  ) where

import Prelude hiding ((!!), read)

import Control.Monad (unless,
                      when)
import Data.Complex
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import qualified Spiral.Array as A
import Spiral.Array (Computable,
                     DS,
                     MArray,
                     SArray,
                     Vector,
                     computeP,
                     forShapeP,
                     indexS,
                     write)
import Spiral.Array.Operators.Matrix
import Spiral.Array.Operators.Permute
import Spiral.Array.Repr.Complex
import Spiral.Array.Repr.Compute
import Spiral.Array.Repr.Concrete
import Spiral.Array.Repr.Slice
import Spiral.Array.Repr.Transform
import Spiral.Array.Shape
import Spiral.Exp
import Spiral.Monad
import Spiral.Program.Monad
import Spiral.Program.Syntax
import Spiral.SPL
import Spiral.Util.Trace

-- | Generate code for an SPL transform.
toProgram :: forall a m . (Num (Exp a), Typed a, MonadSpiral m)
          => String
          -> SPL (Exp a)
          -> m (Program a)
toProgram f a = do
    stms <- evalP $ runSPL a (fromGather x) >>= computeP y
    return $ Program f x y stms
   where
     x, y :: Vector C (Exp a)
     x = C (ix1 n) "X"
     y = C (ix1 m) "Y"

     m, n :: Int
     Z :. m :. n = extent a

infix 4 .<-.
(.<-.) :: forall r1 r2 m a . (MonadSpiral m, MArray r1 DIM1 a, Computable r2 DIM1 a)
       => Vector r1 a
       -> P m (Vector r2 a)
       -> P m ()
y .<-. k = do
    x <- k
    computeP y x

runSPL :: forall m a .
          ( MonadSpiral m
          , Typed a
          , Num (Exp a)
          )
       => SPL (Exp a)
       -> Vector T (Exp a)
       -> P m (Vector T (Exp a))
runSPL e@I{} x = do
    comment $ ppr e
    return x

runSPL e@(Pi p) x = do
    comment $ ppr e
    permuteP p x

runSPL e@(Diag v) x = do
    t <- gather x
    computeTransform e $ \y -> do
      comment $ ppr e
      shouldUnroll n >>= go t y
  where
    n :: Int
    n = V.length v

    go :: forall r1 r2 m . (MonadSpiral m, SArray r1 DIM1 (Exp a), MArray r2 DIM1 (Exp a))
       => Array r1 DIM1 (Exp a)
       -> Array r2 DIM1 (Exp a)
       -> Bool
       -> P m ()
    go x y True =
        forP 0 n $ \ei@(ConstE (IntC i)) ->
            write y (Z :. ei) (v V.! i * indexS x (Z :. ei))

    go x y _ = do
        d <- cacheArray (A.fromFunction (ix1 n) f)
        forShapeP (ix1 n) $ \eix ->
            write y eix (indexS d eix * indexS x eix)
      where
        f (Z :. i) = v V.! i

runSPL e@(KDiag n k) x = do
    t <- gather x
    computeTransform e $ \y -> do
      comment $ ppr e
      forShapeP (ix1 n) $ \eix ->
          write y eix (k * indexS t eix)

runSPL e@(Kron (I m) a) x = do
    t <- gather x
    computeTransform e $ \y -> do
      comment $ ppr e
      forP 0 m $ \i ->
        slice y (i*e_n) 1 n .<-. runSPL a (fromGather (slice t (i*e_n') 1 n'))
  where
    Z :. n :. n' = extent a

    e_n, e_n' :: Exp Int
    e_n  = fromIntegral n
    e_n' = fromIntegral n'

runSPL e@(Kron a (I n)) x = do
    t <- gather x
    computeTransform e $ \y -> do
      comment $ ppr e
      forP 0 n $ \i ->
        slice y i n m .<-. runSPL a (fromGather (slice t i n m'))
  where
    Z :. m :. m' = extent a

runSPL (Kron a b) x =
    runSPL ((I m ⊗ b) × (a ⊗ I n')) x
  where
    Z :. m :. _  = extent a
    Z :. _ :. n' = extent b

runSPL e@(DSum a b) x | m' == m && n' == n = do
    t <- gather x
    computeTransform e $ \y -> do
      comment $ ppr e
      slice y 0 1 m                .<-. runSPL a  (fromGather (slice t 0 1 m))
      slice y (fromIntegral m) 1 n .<-. runSPL b  (fromGather (slice t (fromIntegral m) 1 n))
  where
    Z :. m :. m' = extent a
    Z :. n :. n' = extent b

runSPL e@(Prod a b) x = do
    when (n' /= n) $
        faildoc $ "Incompatible matrix product:" <+> ppr n <+> char 'x' <+> ppr n' </> ppr e
    comment $ ppr e
    t <- runSPL b x
    runSPL a t
  where
    Z :. _m :.  n = extent a
    Z :. n' :. _p = extent b

runSPL (Re a) x = do
    t <- gather x
    computeTransform (Re a) $ \y -> do
      let t' :: Vector (CMPLX DS) (Exp (Complex a)) = toComplexArray t
      toComplexArray y .<-. runSPL a (fromGather t')

runSPL a x = do
    unless (shouldDefault a) $
      traceCg $ text "Falling back to default compilation path:" </> ppr a
    t <- gather x
    computeTransform a $ \y -> do
      comment $ ppr a
      mXvP (toMatrix a) t y
  where
    shouldDefault :: SPL (Exp a) -> Bool
    shouldDefault E{}  = True
    shouldDefault F2{} = True
    shouldDefault _    = False

computeTransform :: MonadSpiral m
                 => SPL a
                 -> (forall r m . (MArray r DIM1 a, MonadSpiral m) => Array r DIM1 a -> P m ())
                 -> P m (Array T DIM1 a)
computeTransform a k =
    return $ fromScatter $ fromCompute (ix1 m) k
  where
    Z :. m :. _n = extent a
