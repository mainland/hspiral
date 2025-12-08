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

import Test.Hspec

import Spiral.NumberTheory

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
            mapM_ opcountSearchTest [2..8]
    opCountTests
    describe "Opcount regressions" $
        opcountRegressionTests 8
