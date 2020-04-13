{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Spiral.Convolution.Winograd (
    winogradR,
    winogradR',
    cyclotomic,

    winogradA,
    winogradB,
    winogradC,

    coreWinogradA,
    coreWinogradB,
    coreWinogradC
  ) where

import Spiral.NumberTheory
import Spiral.RootOfUnity
import Spiral.SPL

import Spiral.Convolution.Core

import Math.NumberTheory.Primes.Testing (isPrime)

import Data.List (foldl1', nub, elemIndices)

-- | R matrix for Winograd convolution
-- | The boolean flag determines which variant of recursive R matrix to select
-- | True = vertical stack variant, False = product variant
winogradR :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> SPL a
winogradR deg = if length (primeFactorization (deg+1)) == 1
                then rpk k
                else stack
  where
    p, k :: Int
    (p, k) = head $ primeFactorization (deg + 1)

    rpk :: Int -> SPL a
    rpk 1  = if p == 2
             then F2
             else ones <-> _Gp
    rpk k' = (rpk (k'-1) ⊕ I ((p-1)*p^(k'-1))) × (rpk 1 ⊗ I (p^(k'-1)))

    _Gp, ones :: SPL a
    _Gp = I (p-1) <|> fromFunction (ix2 (p-1) 1) (\_ -> -1)
    ones  = fromFunction (ix2 1 p) (\_ -> 1)

    stack :: SPL a
    stack = selBurrusMod divs False
      where
        n = deg + 1

        divs :: [Int]
        divs = filter (\i -> n `rem` i == 0) [1..n]

-- | Inverse R matrix for Winograd convolution
winogradR' :: forall a . (RootOfUnity a, Show a, Eq a) => Int -> SPL a
winogradR' deg = if length (primeFactorization (deg+1)) == 1
                 then rpk' k
                 else stack
  where
    p, k :: Int
    (p, k) = head $ primeFactorization (deg + 1)

    p' :: a
    p' = fromIntegral p

    rpk' :: Int -> SPL a
    rpk' 1  = if p == 2 then KDiag p (1/p') × F2 else KDiag p (1/p') × _Vp
    rpk' k' = (rpk' 1 ⊗ I (p^(k'-1))) × (rpk' (k' - 1) ⊕ I ((p-1)*(p^(k'-1))))

    _Vp :: SPL a
    _Vp = fromFunction (ix2 p p) f
     where
       f (Z :. i :. j) | j == 0     = 1
                       | i == (j-1) = p' - 1
                       | otherwise  = -1

    stack :: SPL a
    stack = selBurrusMod divs True
       where
         n = deg + 1

         divs :: [Int]
         divs = filter (\i -> n `rem` i == 0) [1..n]

-- | Generates the nth cyclotomic polynomial
cyclotomic :: Int -> Polynomial Rational
cyclotomic n | n == 1                = construct ([-1, 1] :: [Rational])
             | isPrime (toInteger n) = construct (replicate n 1 :: [Rational])
             | otherwise             = quotient xn (foldl1' mult ws)
  where
    xn =  mono n (1 :: Rational) - (construct [1 :: Rational])
    ws = [cyclotomic i | i <- (filter (\i -> rem n i == 0) [1..(n-1)])]

-- | The A component of the full Winograd convolution
winogradA :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
          => Int
          -> [c a]
          -> SPL a
winogradA n lins = (foldl1' (⊕) $ coreWinogradA lins) × (winogradR (n-1))

-- | The B component of the full Winograd convolution
winogradB :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
          => Int
          -> [c a]
          -> SPL a
winogradB n lins = b' × r' × j'
  where
    b' = foldl1' (⊕) $ coreWinogradB n lins
    r' = transpose $ winogradR' (n-1)
    j' = permute $ J n

-- | The C component of the full Winograd convolution
winogradC :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
          => Int
          -> [c a]
          -> SPL a
winogradC n lins = (permute $ J n) × (transpose $ winogradR (n-1)) × (foldl1' (⊕) $ coreWinogradC lins)

-- | Get the core Winograd A components
coreWinogradA :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
              => [c a]
              -> [SPL a]
coreWinogradA lins = map toCyclicA lins

-- | Get the core Winograd B components
coreWinogradB :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
              => Int
              -> [c a]
              -> [SPL a]
coreWinogradB n lins = map (\(p, lin) -> toCyclicB p lin) $ zip cyclos lins
  where
    cyclos :: [Polynomial Rational]
    cyclos  = [cyclotomic i | i <- (filter (\i -> n `rem` i == 0) [1..n])]

-- | Get the core Winograd C components
coreWinogradC :: forall a c . (RootOfUnity a, Show a, Eq a, Convolution c)
              => [c a]
              -> [SPL a]
coreWinogradC lins = map toCyclicC lins

-- | Produces an R matrix according to the permutation as outlined in
-- | Selesnick and Burrus's paper: Automating the Design of Prime Length FFT Programs
selBurrusMod :: forall a . (RootOfUnity a, Show a, Eq a) => [Int] -> Bool -> SPL a
selBurrusMod xs invert = mtx
  where
    psi :: Int -> Int
    psi 1 = 1
    psi n = minimum [p | (p, _) <- primeFactorization n]

    eta :: Int -> Int -> Int
    eta _ 1 = 1
    eta n a = eta' n a 1

    eta' :: Int -> Int -> Int -> Int
    eta' n a k = if (n `rem` (a^k) == 0)
                 then eta' n a (k+1)
                 else a ^ (k-1)

    g, g' :: [Int]
    g  = xs
    g' = let cd = foldl1 gcd g
         in nub [k `div` cd | k <- g]

    t, _T :: Int
    t  = let t_ = [psi k | k <- g', k > 1] in if null t_ then 1 else minimum t_
    _T = maximum [eta k t | k <- g]

    a, b :: [Int]
    a = filter (\k -> k `mod` _T /= 0) g
    b = filter (\k -> k `mod` _T == 0) g

    forceI :: SPL a -> SPL a
    forceI = (matrix . toMatrix . inverse)

    mtx :: SPL a
    mtx = if null a      then bMat
          else if null b then aMat
          else if length a == 1 && length b == 1
            then if invert then (forceI block') × _P
                           else _P × block'
            else if invert then (forceI block') × (nextA ⊕ nextB) × _P
                           else _P × (nextA ⊕ nextB) × block'
     where
       poly_a, poly_b, poly_f :: Polynomial Rational
       poly_a = foldl1 mult [cyclotomic i | i <- a]
       poly_b = foldl1 mult [cyclotomic i | i <- b]
       poly_f = foldl1 mult [cyclotomic i | i <- (a++b)]

       deg_a, deg_b, deg_f :: Int
       deg_a = ((degree poly_a) - 1)
       deg_b = ((degree poly_b) - 1)
       deg_f = ((degree poly_f) - 1)

       nextA, nextB :: SPL a
       nextA = if length a > 1 then selBurrusMod a invert else aMat
       nextB = if length b > 1 then selBurrusMod b invert else bMat

       aMat, aMat', bMat, bMat', block' :: SPL a
       aMat   = if invert then forceI $ modMatrix poly_a deg_a else modMatrix poly_a deg_a
       aMat'  = modMatrix poly_a deg_f
       bMat   = if invert then forceI $ modMatrix poly_b deg_b else modMatrix poly_b deg_b
       bMat'  = modMatrix poly_b deg_f
       block' = aMat' <-> bMat'

       pPolys, gPolys :: [Polynomial Rational]
       pPolys = map cyclotomic (a++b)
       gPolys = map cyclotomic g

       row_degs, col_degs, new_inds :: [Int]
       row_degs = map degree pPolys -- The new permuted order
       col_degs = map degree gPolys -- The original order
       new_inds = [head $ elemIndices p g | p <- (a ++ b)] -- The permuted index order

       new_pairs :: [(Int, Int)]
       new_pairs = if invert then zip rows cols else zip cols rows
        where
          rows = [0..((length cols)-1)]
          cols = concat [[(sum $ take p col_degs)+y| y <- [0..(r-1)]] | (r,p) <- zip row_degs new_inds]

       _P :: SPL a
       _P = fromFunction (ix2 size size) f
        where
          size = length new_pairs
          f (Z :. x :. y) | (x,y) `elem` new_pairs = 1
                          | otherwise              = 0
