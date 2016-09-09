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

    L(..),
    J(..),

    Permute(..),

    BP,
    idBackpermute
  ) where

import Prelude hiding (read)

import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Program
import Spiral.Exp

-- | A permutation function.
type PermFun = forall a . Integral a => a -> a

-- | An existentially-quantified permutation.
data Perm = forall a . Permutation a => Perm a

-- | A tyep class representing permutations
class (Show a, Pretty a) => Permutation a where
    -- | Convert a permutation into an explicit index-mappoing function.
    fromPermutation :: a -> PermFun

    dim :: a -> Int

    invert :: a -> Perm

instance Show Perm where
    show (Perm p) = show p

instance Pretty Perm where
    ppr (Perm p) = ppr p

instance Permutation Perm where
    fromPermutation (Perm p) = fromPermutation p

    dim (Perm p) = dim p

    invert (Perm p) = invert p

-- See [Voronenko08] p. 24

-- | The $L^{mn}_n$ $mn \times mn$ stride permutation with stride $n$.
data L = L Int Int
  deriving (Eq, Ord, Show)

instance Pretty L where
    ppr (L mn n) = text "L^" <> ppr mn <> char '_' <> ppr n

instance Permutation L where
    fromPermutation (L mn n0) = f
      where
        f :: forall b . Integral b => b -> b
        f i = i `quot` m + n * (i `rem` m)
          where
            m, n :: b
            m = fromIntegral (mn `quot` n0)
            n = fromIntegral n0

    dim (L mn _n) = mn

    invert (L mn n) = Perm (L mn m)
      where
        m = mn `quot` n

-- | The reverse identity permutation.
data J = J Int
  deriving (Eq, Ord, Show)

instance Pretty J where
    ppr (J n) = text "J_" <> ppr n

instance Permutation J where
    fromPermutation (J n0) = f
      where
        f :: forall b . Integral b => b -> b
        f i = n - i
          where
            n :: b
            n = fromIntegral n0

    dim (J n) = n

    invert p@J{} = Perm p

class Permute r where
    permute :: Permutation a => a -> Vector r b -> Vector r b
    permute p = backpermute (invert p)

    backpermute :: Permutation a => a -> Vector r b -> Vector r b
    backpermute p = permute (invert p)

instance Permute D where
    backpermute p (D sh f) = D sh (f . g)
      where
        g (Z :. i) = Z :. fromPermutation p i

instance Permute DS where
    backpermute p (DS sh f) = DS sh (f . g)
      where
        g (Z :. i) = Z :. fromPermutation p i

-- | Type tag for backpermuted matrics
data BP r

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
