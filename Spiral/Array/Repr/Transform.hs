{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Transform
-- Copyright   :  (c) 2016 Drexel University
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
import Spiral.Array.Program
import Spiral.Array.Repr.Compute
import Spiral.Array.Repr.Virtual
import Spiral.Exp

data T

-- | A transform.
instance IsArray T sh a where
    data Array T sh a = G (Array DS sh a)
                      | S (Array P sh a)

    extent (G a) = extent a
    extent (S a) = extent a

instance Compute T sh a where
    computeP y (G x) = computeP y x
    computeP y (S x) = computeP y x

instance Permute T where
    backpermute p (G a) = G (backpermute p a)
    backpermute p a     = permute (invert p) a

    permute p (S a) = S (permute p a)
    permute p a     = backpermute (invert p) a

-- | Ensure that a transform is in a form that can be used as a source.
gather :: (Typed a, Num (Exp a), MonadP m)
       => Vector T (Exp a)
       -> m (Vector DS (Exp a))
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
fromScatter :: Array P sh a -> Array T sh a
fromScatter = S
