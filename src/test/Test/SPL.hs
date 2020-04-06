{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Test.Factorization
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.SPL (
    splTests,
    strideTest,
    l82Test,
    reverseIdentityTest,
    colTest,
    rowTest,
    kroneckerTest,
    directSumTest
  ) where

import qualified Data.Vector as V
import Test.HUnit ((@?=))
import Test.Hspec

import Spiral.Array (M,
                     Matrix)
import qualified Spiral.Array as A
import qualified Spiral.Array.Operators.Matrix as A
import Spiral.SPL

splTests :: Spec
splTests = describe "SPL operations" $ do
    strideTest
    l82Test
    reverseIdentityTest
    colTest
    rowTest
    kroneckerTest
    directSumTest
    transposeTest

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
strideTest :: Spec
strideTest = it "Stride matrix L^8_4" $
    toMatrix (Pi (L 8 4)) @?= a
  where
    a :: Matrix M Int
    a = A.matrix [[1, 0, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 1, 0, 0, 0],
                  [0, 1, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0, 1, 0, 0],
                  [0, 0, 1, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 1, 0],
                  [0, 0, 0, 1, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 0, 1]]

l82Test :: Spec
l82Test = it "Stride matrix L^8_2" $
    toMatrix (Pi (L 8 2)) @?= a
  where
    a :: Matrix M Int
    a = A.matrix [[1, 0, 0, 0, 0, 0, 0, 0],
                  [0, 0, 1, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 1, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 1, 0],
                  [0, 1, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 1, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0, 1, 0, 0],
                  [0, 0, 0, 0, 0, 0, 0, 1]]

reverseIdentityTest :: Spec
reverseIdentityTest = it "Reverse identity matrix J 5" $
    toMatrix (Pi (J 5)) @?= a
  where
    a :: Matrix M Int
    a = A.matrix [[0, 0, 0, 0, 1],
                  [0, 0, 0, 1, 0],
                  [0, 0, 1, 0, 0],
                  [0, 1, 0, 0, 0],
                  [1, 0, 0, 0, 0]]

colTest :: Spec
colTest = it "Extract column" $
    col a 0 @?= v
  where
    a :: Matrix M Int
    a = A.matrix [[0, 1],
                  [2, 5],
                  [5, 1]]

    v :: V.Vector Int
    v = V.fromList [0, 2, 5]

rowTest :: Spec
rowTest = it "Extract row" $
    row a 0 @?= v
  where
    a :: Matrix M Int
    a = A.matrix [[0, 1],
                  [2, 5],
                  [5, 1]]

    v :: V.Vector Int
    v = V.fromList [0, 1]

-- See:
--   https://en.wikipedia.org/wiki/Kronecker_product
kroneckerTest :: Spec
kroneckerTest = it "Kronecker product (⊗)" $
    toMatrix (matrix a ⊗ matrix b) @?= c
  where
    a, b, c :: Matrix M Int
    a = A.matrix [[1, 2],
                  [3, 4]]
    b = A.matrix [[0, 5],
                  [6, 7]]
    c = A.matrix [[0, 5, 0, 10],
                  [6, 7, 12, 14],
                  [0, 15, 0, 20],
                  [18, 21, 24, 28]]

-- See:
--   https://en.wikipedia.org/wiki/Matrix_addition#Direct_sum
directSumTest :: Spec
directSumTest = it "Direct sum (⊕)" $
    toMatrix (matrix a ⊕ matrix b) @?= c
  where
    a, b, c :: Matrix M Int
    a = A.matrix [[1, 3, 2],
                  [2, 3, 1]]
    b = A.matrix [[1, 6],
                  [0, 1]]
    c = A.matrix [[1, 3, 2, 0, 0],
                  [2, 3, 1, 0, 0],
                  [0, 0, 0, 1, 6],
                  [0, 0, 0, 0, 1]]

-- | Test group to verify matrix transposition of different matrix shapes
transposeTest :: Spec
transposeTest = describe "Matrix transpose (manifest)" $ do
    it "Tall transpose" $
        A.manifest (A.transpose tall) @?= tall_t
    it "Wide transpose" $
        A.manifest (A.transpose wide) @?= wide_t
    it "Square transpose" $
        A.manifest (A.transpose square) @?= square_t
    it "Permutation" $
        toMatrix (transpose $ permute (L 16 2)) @?= toMatrix (backpermute (L 16 2) :: SPL Double)
    it "Double transpose" $
        toMatrix (transpose $ transpose $ matrix square) @?= square
  where
    tall, tall_t, wide, wide_t, square, square_t :: Matrix M Int
    tall = A.matrix [[0, 1],
                     [2, 3],
                     [4, 5]]
    tall_t = A.matrix [[0, 2, 4],
                       [1, 3, 5]]
    wide = A.matrix [[2, 1, 0],
                     [3, 5, 6]]
    wide_t = A.matrix [[2, 3],
                       [1, 5],
                       [0, 6]]
    square = A.matrix [[1, 2],
                       [3, 4]]
    square_t = A.matrix [[1, 3],
                         [2, 4]]
