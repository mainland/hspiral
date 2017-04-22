{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Array.Repr.Compute
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Compute (
    CP,
    Array(..),

    toCompute,
    fromCompute,
    mapCompute
  ) where

import Prelude hiding (read)

import Spiral.Array
import Spiral.Array.Operators.Permute
import Spiral.Array.Program

data CP

-- | A computed array. This is like a delayed array, except it embodies a
-- monadic computation that computes the entire array into the provided
-- destination.
instance IsArray CP sh a where
    data Array CP sh a where
      CP :: sh -> (forall r m . (MArray r sh a, MonadP m) => Array r sh a -> m ()) -> Array CP sh a

    extent (CP sh _) = sh

instance Compute CP sh a where
    computeP a (CP _ k) = k a

instance Permute CP where
    permute p (CP sh k) = CP sh (k . backpermute (invert p) . idBackpermute)

toCompute :: (ForShape sh, Compute r sh a) => Array r sh a -> Array CP sh a
toCompute x = fromCompute (extent x) (`computeP` x)

fromCompute :: sh
            -> (forall r m . (MArray r sh a, MonadP m) => Array r sh a -> m ())
            -> Array CP sh a
fromCompute = CP

mapCompute :: forall r' sh a b . MArray r' sh a
           => (sh -> sh)
           -> (forall r . MArray r sh b => Array r sh b -> Array r' sh a)
           -> Array CP sh a
           -> Array CP sh b
mapCompute f g (CP sh k) = CP (f sh) (k . g)
