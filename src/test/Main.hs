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
    factorizationTests
    opCountTests
    describe "Code Generation Tests (default flags)" $
        codegenTests mempty
    describe "Code Generation Tests (full-unrolling)" $
        codegenTests fullUnrollConfig
  where
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
