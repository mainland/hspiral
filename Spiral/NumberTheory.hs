{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.NumberTheory
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.NumberTheory (
    modExp,
    zstar,
    eulerPhi,
    inv,
    euclid,
    generator,

    prop_euclid_gcd,
    prop_inv,
    prop_is_generator,
    prop_generator
  ) where

import Control.Monad.State (StateT(..), State, evalState)
import Data.Bits (xor)
import Data.List (sort)
import Math.NumberTheory.Primes.Factorisation (factorise)
import System.Random
import Test.QuickCheck

-- | Fast modular exponentiation
modExp :: forall a b . (Integral a, Integral b) => a -> b -> a -> a
modExp !_ 0  !_ = 1
modExp !b !e !p = t * modExp ((b * b) `mod` p) (e `quot` 2) p `mod` p
  where
    t :: a
    t | e `rem` 2 == 1 = b
      | otherwise      = 1

-- | Compute $Z*_n$, the set of invertible elements in $Z_n$.
zstar :: Integral a => a -> [a]
zstar n = [x | x <- [0..n-1], gcd x n == 1]

-- | Compute Euler's $\phi$ function, the size of $Z*_n$.
eulerPhi :: Integral a => a -> Int
eulerPhi n = length (zstar n)

-- | Find $n^-1$ in $Z_p$
inv :: Integral a => a -> a -> a
inv p n = s `mod` p
  where
    (s, _t) = euclid n p

-- | The extended Euclidean algorithm. Find @s@ and @t@ such that @s*a + t*b =
-- gcd a b@
euclid :: forall a . Integral a => a -> a -> (a, a)
euclid a b | a < b = (t,s)
  where
    (s,t) = euclid b a

euclid a b = go a b 1 0 0 1
  where
    go a b sa sb ta tb | b == 0    = (sa, ta)
                       | otherwise = go b r sb (sa - q*sb) tb (ta - q*tb)
      where
        (q, r) = a `quotRem` b

-- | Find a generator for $Z_p$ given $p$.
generator :: forall a . (Integral a, Random a) => a -> a
generator p = evalState (go (factorise (fromIntegral p-1))) (mkStdGen $ fromIntegral p `xor` 0xdeadbeef)
  where
    go :: [(Integer, Int)] -> Rand a
    go fs = do
        gammas <- mapM f fs
        return $ product gammas `mod` p

    f :: (Integer, Int) -> Rand a
    f (q, e) = do
        r <- rand
        -- Use mod instead of rem to properly handle negative r
        let a = 1 + r `mod` (p-1)
        let b = modExp a ((p-1) `quot` fromInteger q) p
        if b == 1
          then f (q, e)
          else return $ modExp a ((p-1) `quot` fromInteger (q^e)) p

type Rand a = State StdGen a

rand :: Random a => Rand a
rand = StateT $ \g -> let (x, g') = random g
                      in
                        return (x, g')

prop_euclid_gcd :: Positive Int -> Positive Int -> Bool
prop_euclid_gcd (Positive a) (Positive b) = s*a + t*b == gcd a b
  where
    (s, t) = euclid a b

prop_inv :: Prime Int -> Positive Int -> Property
prop_inv (Prime p) (Positive n0) = n /= 0 ==> (inv p n * n) `mod` p == 1
  where
    n = n0 `mod` p

prop_is_generator :: Integer -> Integer -> Property
prop_is_generator p g = sort [modExp g i p | i <- [0..p-2]] === [1..p-1]

prop_generator :: Prime Integer -> Property
prop_generator (Prime p) = prop_is_generator p g
  where
    g :: Integer
    g = generator p

-- | A prime number
newtype Prime a = Prime a
  deriving (Eq, Ord, Show)

primes :: Integral a => [Prime a]
primes = sieve [2..]
   where
     sieve []     = []
     sieve (p:xs) = Prime p : sieve [x | x <- xs, x `rem` p > 0]

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
    arbitrary = do i <- arbitrary
                   return $ primes Prelude.!! abs i
