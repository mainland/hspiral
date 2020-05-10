{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test.Opcount
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Opcount (
    opCountTests,
    splitRadixOpcountTests,
    difOpcountTests,
    opcountRegressionTests
  ) where

import Control.Monad (mzero)
import Data.Complex
import Data.Typeable (Typeable)
import Test.HUnit (Assertion,
                   (@?=),
                   assertEqual)
import Test.Hspec

import qualified Data.FlagSet as FS

import Spiral (Config(..))
import Spiral.Config
import Spiral.Driver
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.Monad
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Search
import Spiral.Search.FFTBreakdowns
import Spiral.Search.OpCount

import Test.Instances ()

opCountTests :: Spec
opCountTests =
    describe "Opcount" $ do
    splitRadixOpcountTests
    difOpcountTests

-- The number of multiplies and additions for spit radix decomposition of size n
-- when using three-multiply form of complex multiply. Taken from Table II of
-- Heideman and Burrus.
splitRadixOpcounts :: [(Int, Int, Int)]
splitRadixOpcounts = [ (4,    0,     16)
                     , (8,    4,     52)
                     , (16,   20,    148)
                     , (32,   68,    388)
                     , (64,   196,   964)
                     , (128,  516,   2308)
                     , (256,  1284,  5380)
                     --, (512,  3076,  12292)
                     --, (1024, 7172,  27652)
                     --, (2048, 16388, 61444)
                     --, (4096, 36868, 135172)
                     ]

splitRadixOpcountTests :: Spec
splitRadixOpcountTests =
    describe "Split radix opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
    mkTest n nops =
      mkOpCountTest ("Split radix " ++ show n) fs nops (runSearch () f (DFT n))
      where
        fs :: [DynFlag]
        fs = [ StoreIntermediate
             , SplitComplex
             , CSE
             , Rewrite
             ]

    f :: (Typeable a, Typed a, MonadSpiral m)
      => SPL (Exp a)
      -> S s m (SPL (Exp a))
    f (F n w) = splitRadixBreakdown n w
    f _       = mzero

-- The DIF form should have the same operation count as split radix when the
-- DifRewrite flag is enabled.
difOpcountTests :: Spec
difOpcountTests =
    describe "DIF opcounts" $
    sequence_ [mkTest n (muls + adds) | (n, muls, adds) <- splitRadixOpcounts]
  where
    mkTest :: Int -> Int -> Spec
    mkTest n nops =
      mkOpCountTest ("DIF " ++ show n) fs nops (return $ dif n)
      where
        fs :: [DynFlag]
        fs = [ StoreIntermediate
             , SplitComplex
             , CSE
             , Rewrite
             , DifRewrite
             ]

mkOpCountTest :: String
              -> [DynFlag]
              -> Int
              -> Spiral (SPL (Exp (Complex Double)))
              -> Spec
mkOpCountTest desc fs nops gen =
    it desc $ do
    ops <- runSpiralWith mempty $ withOpcountFlags fs $ do
           f   <- gen
           toProgram "f" (Re f) >>= countProgramOps
    return $ mulOps ops + addOps ops @?= nops

withOpcountFlags :: MonadConfig m => [DynFlag] -> m a -> m a
withOpcountFlags fs =
    localConfig $ \env -> env { dynFlags  = FS.fromList fs
                              , maxUnroll = 256
                              }

opcountRegressionTests :: Int -> Spec
opcountRegressionTests max_size = do
    text <- runIO $ readFile rEGRESSION_FILE
    let opcounts = (map parseCSV . drop 1 . lines) text
    mapM_ test [(n,totalOps,mulOps,addOps) | [n,totalOps,mulOps,addOps] <- opcounts, n <= max_size]
  where
    rEGRESSION_FILE :: FilePath
    rEGRESSION_FILE = "benchmark/data/search-opcount.csv"

    parseCSV :: String -> [Int]
    parseCSV = map read . splitOn (== ',')

    test :: (Int,Int,Int,Int) -> Spec
    test (n, allOps0, mulOps0, addOps0) = it ("Regression(" ++ show n ++ ")") $ do
        ops <- runSpiralWith config $ do
               e    <- searchOpCount (Re (DFT n) :: SPL (Exp Double))
               prog <- toProgram "dft" e
               countProgramOps prog
        return $ checkOps (allOps ops, mulOps ops, addOps ops) (allOps0, mulOps0, addOps0)

    checkOps :: (Int, Int, Int) -> (Int, Int, Int) -> Assertion
    checkOps ops@(total, _, _) ops'@(total', _, _)
      | total < total' = assertEqual "IMPROVED!" ops' ops
      | otherwise      = ops @?= ops'

    config :: Config
    config = mempty { dynFlags  = FS.fromList fs
                    , maxUnroll = 256
                    }

    fs :: [DynFlag]
    fs = [ StoreIntermediate
         , SplitComplex
         , CSE
         , Rewrite
         ]

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitOn p s''
                        where (w, s'') = break p s'
