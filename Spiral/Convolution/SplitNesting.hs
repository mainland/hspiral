{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Spiral.Convolution.SplitNesting (
  splitNestingC,
  splitNestingB,
  splitNestingA
  ) where

import Spiral.NumberTheory (euclid)
import Spiral.RootOfUnity
import Spiral.SPL

import Spiral.Convolution.Core (HPoly(..))
import Spiral.Convolution.Winograd

import Data.List (foldl1')


splitNestingA :: forall a . (RootOfUnity a, Show a, Eq a)
              => [Int]
              -> [[SPL a]]
              -> SPL a
splitNestingA ns wins = a × _P × r × _Q
  where
    _Q :: SPL a
    _Q = foldl1' (×) $ map getQPairs $ [(take i ns, drop i ns) |  i <- [2..length ns]]
      where
        getQPairs :: ([Int], [Int]) -> SPL a
        getQPairs (qs, is) = getQ qs ⊗ getI is
          where
            getQ :: [Int] -> SPL a
            getQ xs = let m = product $ take (length xs - 1) xs
                          n = last xs
                          (u, v) = euclid m n
                          em     = n * v
                          en     = m * u
                      in permute $ CRT m n em en

            getI :: [Int] -> SPL a
            getI [] = I 1
            getI xs = I $ product xs

    r :: SPL a
    r = foldl1' (⊗) $ map (\n -> winogradR (n-1)) ns

    _P :: SPL a
    _P = foldl1' (×) $ map getPPairs $ reverse [(take i degs, drop i degs) |  i <- [2..length degs]]
      where
        degs :: [[Int]]
        degs = [[degree $ cyclotomic i | i <- filter (\i -> n `rem` i == 0) [1..n]] | n <- ns]

        getPPairs :: ([[Int]], [[Int]]) -> SPL a
        getPPairs (ps, is) = getP ps ⊗ getI is
          where
            getP :: [[Int]] -> SPL a
            getP xs = let prod = foldl1' mergeDegs $ take (length xs - 1) xs
                          new  = last xs
                      in foldl1' (⊕) [construct_permutation y new | y <- prod]
              where
                construct_permutation :: Int -> [Int] -> SPL a
                construct_permutation 1 xs = I (sum xs)
                construct_permutation y xs = (foldl1' (⊕) [l (y*x) y | x <- xs]) × l (y * sum xs) (sum xs)
                  where
                    l :: Int -> Int -> SPL a
                    l m n | m == n    = I m
                          | otherwise = permute $ L m n

                mergeDegs :: [Int] -> [Int] -> [Int]
                mergeDegs xs ys = [x*y | x <- xs, y <- ys]

            getI :: [[Int]] -> SPL a
            getI [] = I 1
            getI xs = I $ product $ map sum xs

    a :: SPL a
    a = foldl1' (⊕) $ foldl1' combine_components wins
      where
        combine_components :: [SPL a] -> [SPL a] -> [SPL a]
        combine_components xs ys = [x ⊗ y | x <- xs, y <- ys]

splitNestingB :: forall a . (RootOfUnity a, Show a, Eq a)
              => [Int]
              -> [[SPL a]]
              -> SPL a
splitNestingB ns wins = b × _P × rt' × _Q
  where
    _Q :: SPL a
    _Q = let splits = [(take i ns, drop i ns) |  i <- [2..length ns]]
             fs = foldl1' (×) $ map (getQPairs False) $ take (length splits - 1) splits
             ls = getQPairs True $ last splits
         in if length ns > 2 then fs × ls else ls
      where
        getQPairs :: Bool -> ([Int], [Int]) -> SPL a
        getQPairs b (qs, is) = getQ qs b ⊗ getI is

        getQ :: [Int] -> Bool -> SPL a
        getQ xs b = let m = product $ take (length xs - 1) xs
                        n = last xs
                        (u, v) = euclid m n
                        em     = n * v
                        en     = m * u
                  in if b then (permute $ J $ m * n) × (permute $ CRT m n em en) else (permute $ CRT m n em en)

        getI :: [Int] -> SPL a
        getI [] = I 1
        getI xs = I $ product xs

    rt' :: SPL a
    rt' = transpose $ foldl1' (⊗) $ map (\n -> winogradR' (n-1)) ns

    _P :: SPL a
    _P = foldl1' (×) $ map getPPairs $ reverse [(take i degs, drop i degs) |  i <- [2..length degs]]
      where
        degs :: [[Int]]
        degs = [[degree $ cyclotomic i | i <- filter (\i -> n `rem` i == 0) [1..n]] | n <- ns]

        getPPairs :: ([[Int]], [[Int]]) -> SPL a
        getPPairs (ps, is) = getP ps ⊗ getI is
          where
            getP :: [[Int]] -> SPL a
            getP xs = let prod = foldl1' mergeDegs $ take (length xs - 1) xs
                          new  = last xs
                      in foldl1' (⊕) [construct_permutation y new | y <- prod]
              where
                construct_permutation :: Int -> [Int] -> SPL a
                construct_permutation 1 xs = I (sum xs)
                construct_permutation y xs = (foldl1' (⊕) [l (y*x) y | x <- xs]) × l (y * sum xs) (sum xs)
                  where
                    l :: Int -> Int -> SPL a
                    l m n | m == n    = I m
                          | otherwise = permute $ L m n

                mergeDegs :: [Int] -> [Int] -> [Int]
                mergeDegs xs ys = [x*y | x <- xs, y <- ys]

            getI :: [[Int]] -> SPL a
            getI [] = I 1
            getI xs = I $ product $ map sum xs

    b :: SPL a
    b = foldl1' (⊕) $ foldl1' combine_components wins
      where
        combine_components :: [SPL a] -> [SPL a] -> [SPL a]
        combine_components xs ys = [x ⊗ y | x <- xs, y <- ys]

splitNestingC :: forall a . (RootOfUnity a, Show a, Eq a)
              => [Int]
              -> [[SPL a]]
              -> SPL a
splitNestingC ns wins = _Q' × rt × _P' × c
  where
    _Q' :: SPL a
    _Q' = let splits = [(take i ns, drop i ns) |  i <- [2..length ns]]
              fs = foldl1' (×) $ map (getQPairs False) $ take (length splits - 1) splits
              ls = getQPairs True $ last splits
          in if length ns > 2 then ls × fs else ls
      where
      getQPairs :: Bool -> ([Int], [Int]) -> SPL a
      getQPairs b (qs, is) = getQ qs b ⊗ getI is

      getQ :: [Int] -> Bool -> SPL a
      getQ xs b = let m = product $ take (length xs - 1) xs
                      n = last xs
                      (u, v) = euclid m n
                      em     = n * v
                      en     = m * u
                in if b then (backpermute $ CRT m n em en) × (permute $ J $ m * n) else (backpermute $ CRT m n em en)

      getI :: [Int] -> SPL a
      getI [] = I 1
      getI xs = I $ product xs

    rt :: SPL a
    rt = foldl1' (⊗) $ map (\n -> transpose $ winogradR (n-1)) ns

    _P' :: SPL a
    _P' = transpose $ foldl1' (×) $ map getPPairs $ reverse [(take i degs, drop i degs) |  i <- [2..length degs]]
        where
          degs :: [[Int]]
          degs = [[degree $ cyclotomic i | i <- filter (\i -> n `rem` i == 0) [1..n]] | n <- ns]

          getPPairs :: ([[Int]], [[Int]]) -> SPL a
          getPPairs (ps, is) = getP ps ⊗ getI is
            where
              getP :: [[Int]] -> SPL a
              getP xs = let prod = foldl1' mergeDegs $ take (length xs - 1) xs
                            new  = last xs
                        in foldl1' (⊕) [construct_permutation y new | y <- prod]
                where
                  construct_permutation :: Int -> [Int] -> SPL a
                  construct_permutation 1 xs = I (sum xs)
                  construct_permutation y xs = (foldl1' (⊕) [l (y*x) y | x <- xs]) × l (y * sum xs) (sum xs)
                    where
                      l :: Int -> Int -> SPL a
                      l m n | m == n    = I m
                            | otherwise = permute $ L m n

                  mergeDegs :: [Int] -> [Int] -> [Int]
                  mergeDegs xs ys = [x*y | x <- xs, y <- ys]

              getI :: [[Int]] -> SPL a
              getI [] = I 1
              getI xs = I $ product $ map sum xs

    c :: SPL a
    c = foldl1' (⊕) $ foldl1' combine_components wins
      where
        combine_components :: [SPL a] -> [SPL a] -> [SPL a]
        combine_components xs ys = [x ⊗ y | x <- xs, y <- ys]
