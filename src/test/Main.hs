{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (main) where

import Data.List (nub,
                  sort)
import Test.Hspec

import qualified Data.FlagSet as FS
import Spiral.Config
import Spiral.NumberTheory

import Test.Codegen
import Test.Factorization
import Test.Opcount
import Test.SPL

main :: IO ()
main = do
    setGenerator 17 3
    setGenerator 2013265921 31
    hspec spec

spec :: Spec
spec = do
    splTests
    describe "Factorization" $ do
        factorizationTests
        describe "Opcount-optimized DFT" $
            mapM_ opcountSearchTest allSizes
    opCountTests
    describe "Opcount regressions" $
        opcountRegressionTests 32
    describe "Code Generation Tests (default flags)" $ do
        codegenTests mempty pow2Sizes
        searchCodegenTests mempty allSizes
    describe "Code Generation Tests (full-unrolling)" $ do
        codegenTests fullUnrollConfig pow2Sizes
        searchCodegenTests fullUnrollConfig allSizes
  where
    allSizes :: [Int]
    allSizes = nub . sort $ [2..32] ++ pow2Sizes

    pow2Sizes :: [Int]
    pow2Sizes = [2^i | i <- [1..9::Int]]

    fullUnrollConfig :: Config
    fullUnrollConfig = mempty { dynFlags  = FS.fromList fs
                              , maxUnroll = 4096
                              }
      where
      fs :: [DynFlag]
      fs = [ StoreIntermediate
           , SplitComplex
           , CSE
           , Rewrite
           , DifRewrite
           ]
