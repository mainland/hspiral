{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Spiral.NumberTheory
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.NumberTheory (
    primeFactorization,
    factors,
    coprimeFactors,

    modExp,
    zstar,
    eulerPhi,
    inv,
    euclid,
    generator,
    setGenerator,

    prop_euclid_gcd,
    prop_inv,
    prop_is_generator,
    prop_generator
  ) where

import Control.Monad.State (StateT(..), State, evalState)
import Data.Bits (xor)
import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Math.NumberTheory.Primes (Prime, factorise, primes, unPrime)
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Test.QuickCheck

-- | Compute prime factorization,
primeFactorization :: Int -> [(Int, Int)]
primeFactorization x = [(fromInteger (unPrime p), fromIntegral n) | (p,n) <- factorise (fromIntegral x)]

-- | Return all possible ways to factor a number into two factors, neither of
-- which is one.
factors :: Int -> [(Int,Int)]
factors n =
    filter (not . dumbFactor) $
    factorSplits $ primeFactorization n

-- | Return all possible ways to factor a number into two coprime factors,
-- neither of which is one.
coprimeFactors :: Int -> [(Int,Int)]
coprimeFactors n =
    filter (not . dumbFactor)
    [(unfactor fs1, unfactor fs2) | (fs1, fs2) <- splits $ primeFactorization n]

-- | A prime factor we don't want.
dumbFactor :: (Int,Int) -> Bool
dumbFactor (1,_) = True
dumbFactor (_,1) = True
dumbFactor _     = False

-- | Generate all possible splittings of a list, avoiding empty lists.
splits :: [a] -> [([a], [a])]
splits []     = []
splits [x]    = [([x], []), ([], [x])]
splits (x:xs) = [(x:ys, zs) | (ys, zs) <- ss] ++ [(ys, x:zs) | (ys, zs) <- ss]
  where
    ss = splits xs

-- | Given a prime factorization, return all possible ways to split the original
-- number into two factors.
factorSplits :: [(Int,Int)] -> [(Int,Int)]
factorSplits []         = [(1, 1)]
factorSplits ((p,n):fs) = [(p^i*x, p^(n-i)*y) | (x,y) <- factorSplits fs, i <- [0..n]]

-- | Convert a prime fatorization back into a number.
unfactor :: [(Int,Int)] -> Int
unfactor []         = 1
unfactor ((p,n):fs) = p^n*unfactor fs

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

-- | Cache for generators.
gGenerators :: IORef (Map Integer Integer)
{-# NOINLINE gGenerators #-}
gGenerators = unsafePerformIO $ newIORef mempty

-- | Find a generator for $Z_p$ given $p$.
generator :: forall a . (Integral a, Random a) => a -> a
generator p =
    case Map.lookup (fromIntegral p) (unsafePerformIO (readIORef gGenerators)) of
      Just g  -> fromInteger g
      Nothing -> let g = generator' p
                 in
                   unsafePerformIO (modifyIORef gGenerators $ \gs -> Map.insert (fromIntegral p) (fromIntegral g) gs) `seq` g

-- | Set the cached generator for a prime. Naughty!
setGenerator :: Integer -> Integer -> IO ()
setGenerator p g = modifyIORef gGenerators $ \gs -> Map.insert p g gs

-- Do the real work of finding a generator for $Z_p$ given $p$.
generator' :: forall a . (Integral a, Random a) => a -> a
generator' p = evalState (go (factorise (fromIntegral p-1))) (mkStdGen $ fromIntegral p `xor` 0xdeadbeef)
  where
    go :: [(Prime Integer, Word)] -> Rand a
    go fs = do
        gammas <- mapM f fs
        return $ product gammas `mod` p

    f :: (Prime Integer, Word) -> Rand a
    f (q_prime, e) = do
        r <- rand
        -- Use mod instead of rem to properly handle negative r
        let a = 1 + r `mod` (p-1)
        let b = modExp a ((p-1) `quot` fromInteger q) p
        if b == 1
          then f (q_prime, e)
          else return $ modExp a ((p-1) `quot` fromInteger (q^e)) p
      where
        q = unPrime q_prime

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
prop_inv prime (Positive n0) = n /= 0 ==> (inv p n * n) `mod` p == 1
  where
    n = n0 `mod` p
    p = unPrime prime

prop_is_generator :: Integer -> Integer -> Property
prop_is_generator p g = sort [modExp g i p | i <- [0..p-2]] === [1..p-1]

prop_generator :: Prime Integer -> Property
prop_generator prime = prop_is_generator p g
  where
    g :: Integer
    g = generator p

    p = unPrime prime

instance Arbitrary (Prime Integer) where
    arbitrary = do i <- arbitrary
                   return $ ps Prelude.!! abs i
      where
        ps :: [Prime Integer]
        ps = primes
