{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Spiral.Array.Operators.Permute
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Permute (
    Array(..),

    Permutation(..),
    Perm(..),
    L,
    J,

    Permute(..),

    BP,
    idBackpermute
  ) where

import Prelude hiding (read)

import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Exp

-- | A permutation function.
type PermFun = forall a . Integral a => a -> a

-- | A permutation.
class (Show (Perm r), Pretty (Perm r)) => Permutation r where
    data Perm r

    -- | Convert a permutation into an explicit index-mapping function.
    fromPermutation :: Perm r -> PermFun

    dim :: Perm r -> Int

    invert :: Perm r -> Perm r

data L

-- | The $L^{mn}_n$ $mn \times mn$ stride permutation with stride $n$.
instance Permutation L where
    data Perm L = L Int Int
      deriving (Eq, Ord, Show)

    fromPermutation (L mn n0) = f
      where
        -- See [Voronenko08] p. 24
        f :: forall b . Integral b => b -> b
        f i = i `quot` m + n * (i `rem` m)
          where
            m, n :: b
            m = fromIntegral (mn `quot` n0)
            n = fromIntegral n0

    dim (L mn _n) = mn

    invert (L mn n) = L mn m
      where
        m = mn `quot` n

instance Pretty (Perm L) where
    ppr (L mn n) = text "L^" <> ppr mn <> char '_' <> ppr n

data J

-- | The reverse identity permutation.
instance Permutation J where
    data Perm J = J Int
      deriving (Eq, Ord, Show)

    fromPermutation (J n0) = f
      where
        f :: forall b . Integral b => b -> b
        f i = n - i
          where
            n :: b
            n = fromIntegral n0

    dim (J n) = n

    invert p@J{} = p

instance Pretty (Perm J) where
    ppr (J n) = text "J_" <> ppr n

-- | Array representations that can be permuted.
class Permute r where
    permute :: Permutation p => Perm p -> Vector r b -> Vector r b
    permute p = backpermute (invert p)

    backpermute :: Permutation p => Perm p -> Vector r b -> Vector r b
    backpermute p = permute (invert p)

instance Permute D where
    backpermute p (D sh f) = D sh (f . g)
      where
        g (Z :. i) = Z :. fromPermutation p i

instance Permute DS where
    backpermute p (DS sh f) = DS sh (f . g)
      where
        g (Z :. i) = Z :. fromPermutation p i

data BP r

-- | A backpermuted vector.
instance IsArray r DIM1 a => IsArray (BP r) DIM1 a where
    data Array (BP r) DIM1 a = BP PermFun (Array r DIM1 a)

    extent (BP _ a) = extent a

instance IArray r DIM1 a => IArray (BP r) DIM1 a where
    index (BP f a) (Z :. i) = index a (Z :. f i)

instance SArray r DIM1 a => SArray (BP r) DIM1 a where
    indexS (BP f a) (Z :. ConstE (IntC i)) = indexS a (Z :. intE (f i))
    indexS (BP f a) (Z :. i)               = indexS a (Z :. f i)

instance MArray r DIM1 e => MArray (BP r) DIM1 e where
    read  (BP f a) (Z :. ConstE (IntC i)) = read  a (Z :. intE (f i))
    read  (BP f a) (Z :. i)               = read  a (Z :. f i)

    write (BP f a) (Z :. ConstE (IntC i)) = write a (Z :. intE (f i))
    write (BP f a) (Z :. i)               = write a (Z :. f i)

-- XXX Do we want to be able to compute a 'BP r'? It is usually better to
-- permute the destination instead of backpermuting the source, so I've
-- commented out the Compute instance.

{-
instance SArray r DIM1 a => Compute (BP r) DIM1 a where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)
-}

instance Permute (BP r) where
    backpermute p (BP f a) = BP (g . f) a
      where
        g :: PermFun
        g = fromPermutation p

-- | Create a backpermuted vector where the permutation is the identity
-- permutation.
idBackpermute :: Vector r a -> Vector (BP r) a
idBackpermute = BP id
