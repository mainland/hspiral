{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Data.Heterogeneous
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Data.Heterogeneous (
  HEq(..),
  HOrd(..),
  Some(..)
  )
  where

-- | Heterogeneous equality.
class HEq f where
    heq, hne :: f a -> f b -> Bool

    heq x y = not (hne x y)
    hne x y = not (heq x y)

-- | Heterogeneous comparison.
class HEq f => HOrd f where
    hcompare :: f a -> f b -> Ordering

-- | An existential wrapper.
data Some f = forall a . Some (f a)

instance HEq f => Eq (Some f) where
    Some f == Some g = heq f g

instance HOrd f => Ord (Some f) where
    compare (Some f) (Some g) = hcompare f g
