{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.FFTBreakdowns
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.FFTBreakdowns (
    bruteForce,
    cooleyTukeyBreakdowns,
    splitRadixBreakdown,
    splitRadix8Breakdown,
    conjPairSplitRadixBreakdown,
    improvedSplitRadixBreakdown,
    goodThomasBreakdowns,
    raderBreakdowns,
    winogradSmallBreakdowns,
    winogradTwoBreakdowns,
    winogradLargeBreakdowns,
    winogradSquareBreakdowns,
    winogradPowerBreakdowns,
    getCycs,
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus,
                      guard,
                      msum)
import Data.List
import Data.Typeable (Typeable)
import Math.NumberTheory.Primes.Testing (isPrime)

import Spiral.Convolution
import Spiral.Convolution.Winograd (cyclotomic)
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.FFT.GoodThomas (goodThomas)
import Spiral.FFT.Rader (rader, raderI, raderII, raderIII, raderIV, raderLuIII)
import Spiral.FFT.Winograd
import Spiral.NumberTheory (coprimeFactors,
                            factors,
                            primeFactorization)
import Spiral.RootOfUnity
import Spiral.SPL hiding ((<|>))

bruteForce :: (RootOfUnity (Exp a), MonadPlus m)
           => Int
           -> Exp a
           -> m (SPL (Exp a))
bruteForce n w = return $ (matrix . toMatrix) (F n w)

cooleyTukeyBreakdowns :: (RootOfUnity (Exp a), MonadPlus m)
                      => Int
                      -> Exp a
                      -> m (SPL (Exp a))
cooleyTukeyBreakdowns n w =
    msum [return $ cooleyTukeyDIF r s w | (r, s) <- fs] <|>
    msum [return $ cooleyTukeyDIT r s w | (r, s) <- fs]
  where
    fs :: [(Int, Int)]
    fs = factors n

splitRadixBreakdown :: (RootOfUnity (Exp a), MonadPlus m)
                    => Int
                    -> Exp a
                    -> m (SPL (Exp a))
splitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ splitRadix n w

splitRadix8Breakdown :: (RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
splitRadix8Breakdown n w = do
    guard (n `rem` 8 == 0)
    return $ splitRadix8 n w

conjPairSplitRadixBreakdown :: (RootOfUnity (Exp a), MonadPlus m)
                            => Int
                            -> Exp a
                            -> m (SPL (Exp a))
conjPairSplitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    return $ conjPairSplitRadix n w

improvedSplitRadixBreakdown :: forall a m . (Typed a, RootOfUnity (Exp a), MonadPlus m)
                            => Int
                            -> Exp a
                            -> m (SPL (Exp a))
improvedSplitRadixBreakdown n w = do
    guard (n `rem` 4 == 0)
    case tau of
      ComplexT{} -> return $ improvedSplitRadix n w
      _          -> return $ splitRadix n w
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

goodThomasBreakdowns :: (RootOfUnity (Exp a), MonadPlus m)
                     => Int
                     -> Exp a
                     -> m (SPL (Exp a))
goodThomasBreakdowns n w =
    msum [return $ goodThomas r s w | (r, s) <- coprimeFactors n]

raderBreakdowns :: forall a m . (RootOfUnity (Exp a), MonadPlus m)
                => Int
                -> Exp a
                -> m (SPL (Exp a))
raderBreakdowns n w = do
    guard (isPrime (fromIntegral n) && n > 2)
    msum $ map return $ [rader n w, raderII n w, raderIII n w, raderIV n w, raderLuIII n w]
      ++
      [raderI n w cyc | cyc <- getCycs (n-1)]

winogradSmallBreakdowns :: forall a m . (RootOfUnity (Exp a), MonadPlus m)
                   => Int
                   -> Exp a
                   -> m (SPL (Exp a))
winogradSmallBreakdowns n w = do
    guard (isPrime (fromIntegral n) && n > 2)
    msum $ map return $ [winogradSmall n w cyc | cyc <- getCycs (n-1)]

winogradTwoBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                      => Int
                      -> Exp a
                      -> m (SPL (Exp a))
winogradTwoBreakdowns n w = do
    let pfs = primeFactorization n
    let (p, k) = head pfs
    guard (length pfs == 1 && p == 2 && k > 1)
    msum $ map return $ [winogradDIT r s w | (r,s) <- fs]
                        ++
                        [winogradDIF r s w | (r,s) <- fs]
                        ++
                        [winogradSplitRadix n w, winogradConjPairSplitRadix n w]
                        ++
                        if k > 2 then [winogradSplitRadix8 n w] else []
                        ++
                        case tau of
                          ComplexT{} -> [winogradImprovedSplitRadix n w]
                          _          -> [winogradSplitRadix n w]
  where
    fs :: [(Int, Int)]
    fs = factors n

    tau :: Type a
    tau = typeOf (undefined :: a)

winogradLargeBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                        => Int
                        -> Exp a
                        -> m (SPL (Exp a))
winogradLargeBreakdowns n w = do
    case tau of
      ComplexT{} -> msum $ map return [winogradLarge r s w cr cs | (r, s) <- coprimeFactors n, cr <- getWinogradTriple' r s w getCycs, cs <- getWinogradTriple' s r w getCycs]
      _          -> msum $ map return [winogradLarge r s w cr cs | (r, s) <- coprimeFactors n, cr <- getWinogradTriple r s w getCycs, cs <- getWinogradTriple s r w getCycs]
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

winogradPowerBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                        => Int
                        -> Exp a
                        -> m (SPL (Exp a))
winogradPowerBreakdowns n w = do
    guard (length (primeFactorization n) == 1 && (p == 3) && (k == 2))
    msum $ map return $ [winogradPower p k w cP cS | cP <- getCycs p', cS <- getCycs s]
  where
    p, k, p', s :: Int
    (p, k) = head $ primeFactorization n
    p' = (p-1)
    s  = p*p'

winogradSquareBreakdowns :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadPlus m)
                         => Int
                         -> Exp a
                         -> m (SPL (Exp a))
winogradSquareBreakdowns n w = do
    guard (length (primeFactorization n) == 1 && (p > 2) && (k == 2))
    msum $ map return $ winogradSquare p k w getCycs
  where
    p, k :: Int
    (p, k) = head $ primeFactorization n

-- | Gets the cyclic convolutions for a given size
getCycs :: forall a . (RootOfUnity (Exp a))
        => Int
        -> [CyclicConvolution (Exp a)]
getCycs 1 = [ConvolutionTheorem 1]
getCycs x = if (length (primeFactorization x) == 1)
            then [ConvolutionTheorem x]
                 ++
                 getWinograd x
            else [ConvolutionTheorem x]
                 ++
                 [AgarwalCooley r s wr ws | (r, s) <- cofactors, wr <- getCycs r, ws <- getCycs s]
                 ++
                 [SplitNesting ns cycs | ns <- factors, cycs <- map reverse $ foldl' cartProd [[]] $ map (\nsub -> getWinograd nsub) ns]
  where
    cofactors :: [(Int, Int)]
    cofactors = coprimeFactors x
    factors = nub $ permutations $ map (\(p,k) -> p^k) $ primeFactorization x
    cartProd xs ys = [y : x | x <- xs, y <- ys]

    getWinograd x = [Winograd x opt | opt <- linearOptions [degree (cyclotomic i :: Polynomial Rational) | i <- filter (\i -> x `rem` i == 0) [1..x]]]

    linearOptions :: [Int]
                  -> [[LinearConvolution (Exp a)]]
    linearOptions []     = [[]]
    linearOptions (x:xs) = [opt : res | opt <- ls x, res <- linearOptions xs]

    ls :: Int -> [LinearConvolution (Exp a)]
    ls 1 = [Standard 1]
    ls x = if (isPrime (fromIntegral x))
           then [Standard x, ToomCook x]
           else [Standard x, ToomCook x]
                ++
                [Tensor (reverse ns) os | ns <- (nub . permutations $ fs x), os <- linearOpts ns]
      where
        fs :: Int -> [Int]
        fs n = sort $ concat [replicate i p | (p, i) <- primeFactorization n]

        linearOpts :: [Int] -> [[LinearConvolution (Exp a)]]
        linearOpts xs = f' xs [[]]
          where
            f' :: [Int] -> [[LinearConvolution (Exp a)]] -> [[LinearConvolution (Exp a)]]
            f' []     ss = ss
            f' (x:xs) ss = f' xs $ concat [xSelector x s | s <- ss]
              where
                xSelector x s = [o : s | o <- ls x]
