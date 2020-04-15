{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spiral.FFT.Winograd (
  -- Prime algorithms
  winogradSmall,

  -- Power of 2 algorithms
  winogradDIT,
  winogradDIF,
  winogradSplitRadix,
  winogradConjPairSplitRadix,
  winogradSplitRadix8,
  winogradImprovedSplitRadix,

  -- Composite Algorithms
  winogradLarge,

  -- Odd prime powers
  winogradSquare,
  winogradPower,

  getWinogradTriple,
  getWinogradTriple',
  ) where

import Spiral.Array.Base (toLists)
import Spiral.Convolution
import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL

import qualified Data.Set as Set

default (Int)

-- | A 1x1 matrix of just a 1
one :: (RootOfUnity a, Show a) => SPL a
one = I 1

-- | More efficient nub function for creating a list of unique values
nub' :: (Ord a) => [a] -> [a]
nub' = func Set.empty
  where
    func _ [] = []
    func set (x:xs) | Set.member x set = func set xs
                    | otherwise = x : func (Set.insert x set) xs

getWinogradTriple :: forall a . (RootOfUnity a, Show a, Eq a)
                  => Int
                  -> Int
                  -> a
                  -> (Int -> [CyclicConvolution a])
                  -> [(SPL a, SPL a, SPL a)]
getWinogradTriple x y w cycGen =
     if (length pfs == 1)         -- Dealing with a prime (or prime power)
     then if p == 2               -- Dealing with powers of 2
          then let (_, v) = euclid x y
                   em     = v * y
                   allF2s = [biWinogradDIT, biWinogradDIF]
                   allF4s = [biWinogradSplitRadix, biWinogradConjPairSplitRadix]
                   allF8s = biWinogradSplitRadix8 : allF4s
                   w2     = w ^^ em
               in if k == 1
                  then map (\f -> f 2 1 w2) allF2s
                  else if k == 2
                       then (map (\f -> f x w2) allF4s) ++ (concat [map (\f -> f r s w2) allF2s | (r, s) <- factors x]) -- Powers of 2
                       else (map (\f -> f x w2) allF8s) ++ (concat [map (\f -> f r s w2) allF2s | (r, s) <- factors x])
          else if k == 1
               then [biWinogradSmall x c | c <- cycGen (x-1)] -- Odd primes
               else [biWinogradPower p k cP cS | p == 3, k == 2, cP <- cycGen (p-1), cS <- cycGen ((p-1)*p)] -- Size 9 for now
     else [biWinogradLarge r s cr cs | (r,s) <- coprimeFactors x, cr <- getWinogradTriple r s w cycGen, cs <- getWinogradTriple s r w cycGen] -- Recursive options, composite case
  where
    pfs = primeFactorization x
    (p, k) = head $ pfs

getWinogradTriple' :: forall a . (RootOfUnity a, Show a, Eq a, Floating a)
                   => Int
                   -> Int
                   -> a
                   -> (Int -> [CyclicConvolution a])
                   -> [(SPL a, SPL a, SPL a)]
getWinogradTriple' x y w cycGen =
     if (length pfs == 1)         -- Dealing with a prime (or prime power)
     then if p == 2               -- Dealing with powers of 2
          then let (_, v) = euclid x y
                   em     = v * y
                   allF2s = [biWinogradDIT, biWinogradDIF]
                   allF4s = [biWinogradSplitRadix, biWinogradConjPairSplitRadix, biWinogradImprovedSplitRadix]
                   allF8s = biWinogradSplitRadix8 : allF4s
                   w2     = w ^^ em
               in if k == 1
                  then map (\f -> f 2 1 w2) allF2s
                  else if k == 2
                       then (map (\f -> f x w2) allF4s) ++ (concat [map (\f -> f r s w2) allF2s | (r, s) <- factors x]) -- Powers of 2
                       else (map (\f -> f x w2) allF8s) ++ (concat [map (\f -> f r s w2) allF2s | (r, s) <- factors x])
          else if k == 1
               then [biWinogradSmall x c | c <- cycGen (x-1)] -- Odd primes
               else [biWinogradPower p k cP cS | p == 3, k == 2, cP <- cycGen (p-1), cS <- cycGen ((p-1)*p)] -- Size 9 for now
     else [biWinogradLarge r s cr cs | (r,s) <- coprimeFactors x, cr <- getWinogradTriple' r s w cycGen, cs <- getWinogradTriple' s r w cycGen] -- Recursive options, composite case
  where
    pfs = primeFactorization x
    (p, k) = head $ pfs

-- | The bilinear factorization of a prime FFT
-- | per Selesnick+Burrus in their automation of prime FFT paper
biWinogradSmall :: forall a . (RootOfUnity a, Show a, Eq a)
                => Int
                -> CyclicConvolution a
                -> (SPL a, SPL a, SPL a)
biWinogradSmall p conv = (cW, bW, aW)
  where
    a, a' :: Int
    a = generator p
    a' = inv p a

    qS, qR, qR' :: SPL a
    qS  = backpermute (R p a')
    qR  = permute (R p a)
    qR' = backpermute (R p a)

    u, v, ut :: SPL a
    u  = fromLists [[1, 1],
                    [0, 1]]
    v  = fromLists [[ 1, 0],
                    [-1, 1]]
    ut = fromLists [[1, 0],
                    [1, 1]]

    up, vp, upt :: SPL a -> SPL a
    up  x = let (Z :. m :. _) = extent x
            in u ⊕ I (m-1)
    vp  x = let (Z :. m :. _) = extent x
            in v ⊕ I (m-1)
    upt x = let (Z :. m :. _) = extent x
            in ut ⊕ I (m-1)

    aW, bW, cW :: SPL a
    aW = (up (getB conv)) × (one ⊕ (getA conv)) × qS
    bW = (vp (getB conv)) × (one ⊕ (getB conv)) × qR'
    cW = qR × (one ⊕ (getC conv)) × (upt (getB conv))

-- | Given a bilinear algorithm for computing cyclic convolution of (p-1)
-- | produces a Winograd FFT of size p, where p is prime
winogradSmall :: forall a . (RootOfUnity a, Show a, Eq a)
              => Int
              -> a
              -> CyclicConvolution a
              -> SPL a
winogradSmall p w conv = let (c, b, a) = biWinogradSmall p conv
                         in bilinear omegas c b a
  where
    omegas :: [a]
    omegas = [w^i | i <- [0..p-1]]

-- | Twiddle factor matrix $T^{rs}_s(\omega)$
twid :: RootOfUnity a => Int -> Int -> a -> SPL a
twid rs s w = foldr1 (⊕) [ws s (w^i) | i <- [0..r-1]]
 where
   r = rs `quot` s

-- | The $W_n(\omega)$ matrix
ws :: RootOfUnity a => Int -> a -> SPL a
ws n w = diag [w^i | i <- [0..n-1]]

winogradDIT :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> SPL a
winogradDIT r s w = let (c, b, a) = biWinogradDIT r s w
                    in bilinear os c b a
  where
    n = r*s

    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradDIT :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> (SPL a, SPL a, SPL a)
biWinogradDIT 2 1 w = biWinogradDIT 1 2 w
biWinogradDIT 1 2 w = (c, b, a)
 where
   c, b, a :: SPL a
   a = F2
   b = diag [1, 1/w]
   c = I 2
biWinogradDIT r s w = (c, b, a)
  where
    n = r*s
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    c, b, a :: SPL a
    c = (F r (w^s) ⊗ I s)
    b = twid (r*s) s w × diag os
    a = (I r ⊗ F s (w^r)) × Pi (L (r*s) r)

winogradDIF :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> SPL a
winogradDIF r s w = let (c, b, a) = biWinogradDIF r s w
                    in bilinear os c b a
  where
    n = r*s

    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradDIF :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> Int -> a -> (SPL a, SPL a, SPL a)
biWinogradDIF 1 2 w = biWinogradDIF 2 1 w
biWinogradDIF 2 1 w = (c, b, a)
  where
    c, b, a :: SPL a
    a = I 2
    b = diag [1, 1/w]
    c = F2
biWinogradDIF r s w = (c, b, a)
  where
    n = r*s
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    c, b, a :: SPL a
    c = Pi (L (r*s) s) × (I r ⊗ F s (w^r))
    b = twid (r*s) s w × diag os
    a = (F r (w^s) ⊗ I s)

winogradSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradSplitRadix n w = let (c, b, a) = biWinogradSplitRadix n w
                         in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ ws p w ⊕ ws p (w^3)) × diag os
    a = (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)


winogradConjPairSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradConjPairSplitRadix n w = let (c, b, a) = biWinogradConjPairSplitRadix n w
                                 in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradConjPairSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradConjPairSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradConjPairSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ ws p w ⊕ ws p (w^^(-1))) × diag os
    a = (F (2*p) (w^2) ⊕ F p (w^4) ⊕ F p (w^4)×Pi (CS p 1)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)


winogradSplitRadix8 :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> SPL a
winogradSplitRadix8 n w = let (c, b, a) = biWinogradSplitRadix8 n w
                          in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradSplitRadix8 :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradSplitRadix8 n _ | n `mod` 8 /= 0 =
    error "Cannot call splitRadix8 when n is not divisible by 8"
biWinogradSplitRadix8 n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 8

    sigma8 :: SPL a
    sigma8 = (F2 ⊗ I 4) × (I 4 ⊕ (ws 4 w8 × (F2 ⊗ I 2))) × (I 6 ⊕ (ws 2 w4 × F2))

    w4, w8 :: a
    w4 = w ^ (n `div` 4)
    w8 = w ^ (n `div` 8)

    c, b, a :: SPL a
    c = (sigma8 ⊗ I p)
    b = (I (4*p) ⊕ ws p w ⊕ ws p w ⊕ ws p (w^3) ⊕ ws p (w^7)) × diag os
    a = (F (4*p) (w^2) ⊕ F (2*p) (w^4) ⊕ F p (w^8) ⊕ F p (w^8)) ×
        (I (6*p) ⊕ Pi (L (2*p) 2)) ×
        (I (4*p) ⊕ Pi (L (4*p) 2)) ×
        Pi (L (8*p) 2)

winogradImprovedSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a, Floating a) => Int -> a -> SPL a
winogradImprovedSplitRadix n w = let (c, b, a) = biWinogradImprovedSplitRadix n w
                                 in bilinear os c b a
  where
    os :: [a]
    os = [w^i | i <- [0..n-1]]

biWinogradImprovedSplitRadix :: forall a . (RootOfUnity a, Show a, Eq a, Floating a) => Int -> a -> (SPL a, SPL a, SPL a)
biWinogradImprovedSplitRadix n _ | n `mod` 4 /= 0 =
    error "Cannot call splitRadix when n is not divisible by 4"
biWinogradImprovedSplitRadix n w = (c, b, a)
  where
    os = [1/(w^i) | i <- ([0..n-1] :: [Int])]

    p :: Int
    p = n `quot` 4

    sigma4 :: SPL a
    sigma4 = (F2 ⊗ I 2) × (I 2 ⊕ (ws 2 w4 × F2))

    w4 :: a
    w4 = w ^ (n `div` 4)

    s :: Floating a => Int -> Int -> a
    s n k
      | n  <= 4         = 1
      | k4 <= n `div` 8 = s (n `div` 4) k4 * cos (2 * pi * fromIntegral k4 / fromIntegral n)
      | otherwise       = s (n `div` 4) k4 * sin (2 * pi * fromIntegral k4 / fromIntegral n)
      where
        k4 :: Int
        k4 = k `mod` (n `div` 4)

    c, b, a :: SPL a
    c = (sigma4 ⊗ I p)
    b = (I (2*p) ⊕ diag [w^k * s p k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k | k <- [0..p-1]]) × diag os
    a = (F (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4) × Pi (CS p 1)) × (I (2*p) ⊕ Pi (L (2*p) 2)) × Pi (L (4*p) 2)
      where
      fs, fs2, fs4 :: Int -> a -> SPL a
      fs n w | n `mod` 4 /= 0 =
        diag [1/s n k | k <- [0..n-1]] × F n w

      fs n w =
          (sigma4 ⊗ I p) ×
          (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
          (fs2 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
          (I (2*p) ⊕ Pi (L (2*p) 2)) ×
          Pi (L (4*p) 2)
        where
          p :: Int
          p = n `quot` 4

      fs2 n w | n `mod` 4 /= 0 =
        diag [1/s (2*n) k | k <- [0..n-1]] × F n w

      fs2 n w =
          (F2 ⊗ I (2*p)) ×
          (I (2*p) ⊕ diag [s (4*p) k / s (2*4*p) k | k <- [0..2*p-1]]) ×
          ((I 2 ⊕ (ws 2 w4 × F2)) ⊗ I p) ×
          (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
          (fs4 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
          (I (2*p) ⊕ Pi (L (2*p) 2)) ×
          Pi (L (4*p) 2)
        where
          p :: Int
          p = n `quot` 4

      fs4 n w | n `mod` 4 /= 0 =
        diag [1/s (4*n) k | k <- [0..n-1]] × F n w

      fs4 n w =
          diag [s (4*p) k / s (4*4*p) k | k <- [0..(4*p)-1]] ×
          (sigma4 ⊗ I p) ×
          (I (2*p) ⊕ diag [w^k * s p k / s (4*p) k | k <- [0..p-1]] ⊕ diag [w^^(-k) * s p k / s (4*p) k | k <- [0..p-1]]) ×
          (fs2 (2*p) (w^2) ⊕ fs p (w^4) ⊕ fs p (w^4)×Pi (CS p 1)) ×
          (I (2*p) ⊕ Pi (L (2*p) 2)) ×
          Pi (L (4*p) 2)
        where
          p :: Int
          p = n `quot` 4

biWinogradLarge :: forall a . (RootOfUnity a, Show a, Eq a)
                => Int
                -> Int
                -> (SPL a, SPL a, SPL a)
                -> (SPL a, SPL a, SPL a)
                -> (SPL a, SPL a, SPL a)
biWinogradLarge p1 p2 (c1, b1, a1) (c2, b2, a2) = (_Q' × (c1 ⊗ c2), (b1 ⊗ b2) × _Q, (a1 ⊗ a2) × _Q)
  where
    (u1, v1) = euclid p1 p2
    em     = p2 * v1
    en     = p1 * u1

    pi :: Permutation
    pi = CRT p1 p2 em en

    _Q :: SPL a
    _Q = Pi pi

    _Q' :: SPL a
    _Q' = Pi (invert pi)

winogradLarge :: forall a . (RootOfUnity a, Show a, Eq a)
              => Int
              -> Int
              -> a
              -> (SPL a, SPL a, SPL a)
              -> (SPL a, SPL a, SPL a)
              -> SPL a
winogradLarge p1 p2 w t1 t2 = bilinear omegas c b a
  where
    p :: Int
    p = p1 * p2

    omegas :: [a]
    omegas = [w^i | i <- [0..p-1]]

    c, b, a :: SPL a
    (c, b, a) = biWinogradLarge p1 p2 t1 t2

-- | Winograd format for odd prime powers
winogradPower :: forall a . (RootOfUnity a, Show a, Eq a)
              => Int
              -> Int
              -> a
              -> CyclicConvolution a
              -> CyclicConvolution a
              -> SPL a
winogradPower p k w convP convS = bilinear omegas c b a
  where
    omegas    = [w^i | i <- [0..(p^k)-1]]

    c, b, a :: SPL a
    (c, b, a) = biWinogradPower p k convP convS

block' :: [[SPL a]] -> SPL a
block' xss = foldr1 (<->) [foldr1 (<|>) xs | xs <- xss]

biWinogradPower :: forall a . (RootOfUnity a, Show a, Eq a)
                => Int
                -> Int
                -> CyclicConvolution a
                -> CyclicConvolution a
                -> (SPL a, SPL a, SPL a)
biWinogradPower p k convP convS = if (not (p == 3 && k == 2))
                                  then error "Winograd Prime Power currently only works for 3^2"
                                  else (_G' × c', b' × _Gw, a' × _G)
  where
    n_, g_, g_' :: Integer
    n_  = ((toInteger p) ^ k)
    g_  = gen 2
    g_' = inv n_ g_

    n, p', s :: Int
    n  = (p ^ k)
    p' = (p - 1)
    s  = (p * p')

    gen :: Integer -> Integer
    gen z = if length lis == s then z else gen (z+1)
      where
        lis = nub' [(z^k) `mod` n_ | k <- [0..(s-1)]]

    d0, d1, d2, d0' :: [Integer]
    d0  = [(g_^k) `mod` n_ | k <- [0..(s-1)]]
    d0' = [(g_'^k) `mod` n_ | k <- [0..(s-1)]]
    d1  = [(toInteger p) * (toInteger k) `mod` n_ | k <- [1..(p')]]
    d1' = [(toInteger p) * (g_'^k) `mod` n_ | k <- [0..(p'-1)]]
    d2  = [0]

    r, c :: Int -> a -> SPL a
    r l v = fromFunction (ix2 1 l) (\_ -> v)
    c l v = fromFunction (ix2 l 1) (\_ -> v)

    e, et :: Int -> a -> SPL a
    e  n val = fromFunction (ix2 n 1) f
      where
        f (Z :. x :. _) | x == 0    = val
                        | otherwise = 0
    et n val = fromFunction (ix2 1 n) f
      where
        f (Z :. _ :. y) | y == 0    = val
                        | otherwise = 0

    d, d' :: [(Int, Int)]
    d = zip [0..(n-1)] $ map fromInteger (d2 ++ d1 ++ d0)
    d' = zip [0..(n-1)] $ map fromInteger (d2 ++ d1' ++ d0')

    -- | TODO: Transform these prime power permutations into something more
    -- | visually compact instead of arbitrary matrices
    _G, _G', _Gw :: SPL a
    _G = fromFunction (ix2 n n) f
      where
        f (Z :. i :. j) | (i, j) `elem` d' = 1
                        | otherwise        = 0
    _G' = transpose _Gw
    _Gw = fromFunction (ix2 n n) f
      where
        f (Z :. i :. j) | (i, j) `elem` d = 1
                        | otherwise       = 0

    -- | The commented out versions are the alternate variants of the expansion
    -- | and recombination matrices. If switching to these, the cycS must use
    -- | p' p split nesting instead of p p'
    _Ex, _Rx :: Int -> Bool -> SPL a -- The expansion and recombination matrices
    _Ex m' False = I p
                    ⊕ block' [[I p' ⊗ (et (m' `div` p') 1)],
                              [I m']]
    _Ex m' True  = I p
                    ⊕ block' [[block' [[I p', c p' 0 ⊗ r (m' - p') 0]]],
                              [I m']]
    _Rx m' False = one ⊕ block' [[c p' 0 ⊗ r p' 0,            I p',            c p' 0 ⊗ r m' 0],
                                 [I p' ⊗ (e (m' `div` p') 1), c m' 0 ⊗ r p' 0, I m'           ]]
    _Rx m' True = one ⊕ block' [[c p' 0 ⊗ r p' 0,                        I p',            c p' 0 ⊗ r m' 0],
                                [block' [[I p'],
                                         [c (m' - p') 0 ⊗ r p' 0]], c m' 0 ⊗ r p' 0, I m'           ]]

    _U, _Ut, _V :: Int -> Int -> SPL a
    _U  n' m' = block' [[one,           block' [[(et n' 1), (et m' 1)]]],
                        [e (n' + m') 0, I (n' + m')               ]]
    _Ut n' m' = transpose (_U n' m')
    _V  n' m' = block' [[one,                 et (2 * n' + m') 0],
                        [block' [[e n' (-1)],
                                 [e n' (-1)],
                                 [e m' (-1)]], I (2 * n' + m')   ]]

    -- | This function checks for different convolution forms dynamically
    -- | to switch between different expansion and reduction forms
    check_a :: SPL a -> Bool
    check_a a = and [if even i then e == 1 else e == (-1) | (i, e) <- zip ([0..] :: [Int]) ((toLists $ toMatrix a) !! 1)]

    aP, bP, cP, aS, bS, cS :: SPL a
    (cP, bP, aP) = (getC convP, getB convP, getA convP)
    (cS, bS, aS) = (getC convS, getB convS, getA convS)

    c', b', a' :: SPL a
    (c', b', a') = let check          = check_a aS
                       (Z :. m' :. _) = extent aS
                       (Z :. n' :. _) = extent aP
                   in ((one ⊕ cP ⊕ cS) × (_Ut n' m') × (_Rx m' check),
                       (_V n' m') × (one ⊕ bP ⊕ bP ⊕ bS) × (one ⊕ (block' [[(I p')], [(I p')]]) ⊕ I s),
                       (_Ex m' check) × (_U n' m') × (one ⊕ aP ⊕ aS))

-- | Winograd format for odd prime powers
winogradSquare :: forall a . (RootOfUnity a, Show a, Eq a)
               => Int
               -> Int
               -> a
               -> (Int -> [CyclicConvolution a])
               -> [SPL a]
winogradSquare p k w cycGen = [bilinear omegas c b a | (c, b, a) <- biWinogradSquare p k cycGen]
  where
   omegas = [w^i | i <- [0..(p^k)-1]]

biWinogradSquare :: forall a . (RootOfUnity a, Show a, Eq a)
                 => Int
                 -> Int
                 -> (Int -> [CyclicConvolution a])
                 -> [(SPL a, SPL a, SPL a)]
biWinogradSquare p k cycGen =
    [(c' c1 c2 c3, b' b1 b2 b3, a' a1 a2 a3) | (c1, b1, a1) <- w3, (c2, b2, a2) <- w3, (c3, b3, a3) <- c6]
  where
    n, p', s :: Int
    n = p ^ k
    p' = p - 1
    s = p' * p

    n_, g_, g_' :: Integer
    p_  = toInteger p
    n_  = ((toInteger p) ^ k)
    g_  = gen 2
    g_' = inv n_ g_

    gen :: Integer -> Integer
    gen z = if length lis == s then z else gen (z+1)
      where
        lis = nub' [(z^k) `mod` n_ | k <- [0..(s-1)]]

    d0, d2, d0' :: [Integer]
    d0  = [(g_^k) `mod` n_ | k <- [0..(s-1)]]
    d0' = [(g_'^k) `mod` n_ | k <- [0..(s-1)]]
    d1a  = [(toInteger p) * (toInteger k) `mod` n_ | k <- [1..(p')]]
    d1b  = map (\x -> x - 1) [((toInteger p) * (g_^k) `div` p_) `mod` p_ | k <- [0..(p'-1)]]
    d2  = [0]

    dx, dw, dy, dp :: [(Int, Int)]
    dx = zip [0..] (map fromIntegral $ d2 ++ d1a ++ d0')
    dw = zip [0..] (map fromIntegral $ d2 ++ d1a ++ d0)
    dy = zip (map fromIntegral $ d2 ++ d1a ++ d0) [0..]
    dp = zip [0..] (map fromIntegral d1b)

    _Pp, _Px, _Pw, _Py :: SPL a
    _Pp = fromFunction (ix2 p' p') f
      where
        f (Z :. i :. j) | (i, j) `elem` dp = 1
                        | otherwise        = 0
    _Px = fromFunction (ix2 n n) f
      where
        f (Z :. i :. j) | (i, j) `elem` dx = 1
                        | otherwise        = 0
    _Py = fromFunction (ix2 n n) f
      where
        f (Z :. i :. j) | (i, j) `elem` dy = 1
                        | otherwise        = 0
    _Pw = fromFunction (ix2 n n) f
      where
        f (Z :. i :. j) | (i, j) `elem` dw = 1
                        | otherwise        = 0

    w3, c6 :: [(SPL a, SPL a, SPL a)]
    w3 = map (biWinogradSmall p) $ cycGen p'
    c6 = map (\c -> (getC c, getB c, getA c)) $ cycGen s

    a', b', c' :: SPL a -> SPL a -> SPL a -> SPL a
    a' a1 a2 a3 = (a1 ⊕ a2 ⊕ a3) × (block' [[((fromLists [replicate p 1]) ⊗ I p)], [(_Px)]])
    b' b1 b2 b3 = (b1 ⊕ b2 ⊕ b3) × (block' [[(I p)], [(I p)]] ⊕ I s) × _Pw
    c' c1 c2 c3 = _Py ×
                  (I p ⊕ block' [[((fromLists (replicate p [1])) ⊗ I p'), (I s)]]) ×
                  (I (p + p') ⊕ c3) ×
                  (c1 ⊕ (_Pp × block' [[(fromLists $ replicate p' [0]), (I p')]] × c2) ⊕ i3)
      where
        i3  = let (Z :. _ :. j) = extent c3
              in I j
