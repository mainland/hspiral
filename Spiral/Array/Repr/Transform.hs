{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Transform
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Transform (
    Array(..),

    T,

    gather,
    fromGather,
    fromScatter
  ) where

import Prelude hiding (read)

import Spiral.Array
import Spiral.Array.Operators.Permute
import Spiral.Array.Repr.Compute
import Spiral.Array.Repr.Virtual
import Spiral.Exp
import Spiral.Permutation
import Spiral.Program.Monad
import Spiral.Monad

data T

-- | A transform.
instance IsArray T sh a where
    data Array T sh a = G (Array DS sh a)
                      | S (Array CP sh a)

    extent (G a) = extent a
    extent (S a) = extent a

instance Computable T sh a where
    computeP y (G x) = computeP y x
    computeP y (S x) = computeP y x

instance Permute T where
    permuteP p (S a) = S <$> permuteP p a
    permuteP p a     = backpermuteP (invert p) a

    backpermuteP p (G a) = G <$> backpermuteP p a
    backpermuteP p a     = permuteP (invert p) a

-- | Ensure that a transform is in a form that can be used as a source.
gather :: (Typed a, Num (Exp a), MonadSpiral m)
       => Vector T (Exp a)
       -> P m (Vector DS (Exp a))
gather (G x) =
    return x

gather (S x) =
    shouldUnroll n >>= go
  where
    Z :. n = extent x

    -- We know we are unrolling, so we can coerce the result of freezing our
    -- temporary array to symbolic form.
    go True = do
        t <- replicateV (ix1 n) 0
        computeP t x
        t' <- freezeV t
        return $ coerceSymbolic t'

    go _ = do
        t <- newArray (ix1 n)
        computeP t x
        return $ delayS t

-- | Create a transform from a source array.
fromGather :: (Shape sh, SArray r sh a) => Array r sh a -> Array T sh a
fromGather = G . delayS

-- | Create a transform from a computed array.
fromScatter :: Array CP sh a -> Array T sh a
fromScatter = S
