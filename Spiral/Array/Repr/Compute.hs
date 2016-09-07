{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Compute
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Compute (
    P,
    Array(..),

    toCompute,
    fromCompute,
    mapCompute
  ) where

import Prelude hiding (read)

import Spiral.Array
import Spiral.Array.Operators.Permute
import Spiral.Array.Program

data P

-- | A computed array. This is like a delayed array, except it embodies a
-- monadic computation that computes the entire array into the provided
-- destination.
instance IsArray P sh a where
    data Array P sh a where
      P :: sh -> (forall r m . (MArray r sh a, MonadP m) => Array r sh a -> m ()) -> Array P sh a

    extent (P sh _) = sh

instance Compute P sh a where
    computeP a (P _ k) = k a

instance Permute P where
    permute p (P sh k) = P sh (k . backpermute (invert p) . idBackpermute)

toCompute :: (Shape sh, ForShape sh, Compute r sh a) => Array r sh a -> Array P sh a
toCompute x = fromCompute (extent x) (`computeP` x)

fromCompute :: sh
            -> (forall r m . (MArray r sh a, MonadP m) => Array r sh a -> m ())
            -> Array P sh a
fromCompute = P

mapCompute :: forall r' sh a b . MArray r' sh a
           => (sh -> sh)
           -> (forall r . MArray r sh b => Array r sh b -> Array r' sh a)
           -> Array P sh a
           -> Array P sh b
mapCompute f g (P sh k) = P (f sh) (k . g)
