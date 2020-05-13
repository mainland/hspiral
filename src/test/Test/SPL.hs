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
    directSumTest,
    transposeTest
  ) where

import Control.Monad (replicateM)
import qualified Data.Vector as V
import Test.HUnit ((@?=))
import Test.Hspec
import Test.QuickCheck

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

instance Arbitrary Permutation where
    arbitrary = sized $ \n ->
        oneof [ do m <- choose (1, n)
                   pure $ L (m*n) n
              , pure $ J n
              ]

instance (Num a, Arbitrary a) => Arbitrary (SPL a) where
    arbitrary = sized $ \n0 -> do
        let n = n0 + 1
        oneof [ fromLists <$> replicateM n (replicateM n arbitrary)
              , pure $ I n
              , T <$> resize n arbitrary
              , Diag . V.fromList <$> replicateM n arbitrary
              , Circ . V.fromList <$> replicateM n arbitrary
              , Skew . V.fromList <$> replicateM n arbitrary
              , Toep . V.fromList <$> replicateM (2*n-1) arbitrary
              ]

    shrink (I n) = [I (n-1)]
    shrink (T a) = a : map T (shrink a)
    shrink (Diag xs)
      | V.length xs > 0 = map (Diag . V.fromList) (shrink (V.toList xs))
    shrink (Circ xs)
      | V.length xs > 0 = map (Circ . V.fromList) (shrink (V.toList xs))
    shrink (Skew xs)
      | V.length xs > 0 = map (Skew . V.fromList) (shrink (V.toList xs))
    shrink (Toep xs)
      | V.length xs > 3 = map (Toep . V.fromList) (concatMap shrink (shrink (V.toList xs)))
    shrink _     = []

-- | Test group to verify matrix transposition of different matrix shapes
transposeTest :: Spec
transposeTest = describe "Matrix transpose (manifest)" $ do
    it "Permutation" $
        toMatrix (transpose $ permute (L 16 2)) @?= toMatrix (backpermute (L 16 2) :: SPL Double)
    it "Transpose commutes" $
        property (prop_transpose_commutes :: SPL Double -> Property)
    it "Transpose is an involution" $
        property (prop_transpose_involution :: SPL Double -> Property)
  where
    -- | Transpostion commutes with matrix conversion
    prop_transpose_commutes :: (Eq a, Num a, Show a) => SPL a -> Property
    prop_transpose_commutes a = toMatrix (transpose a) === A.manifest (A.transpose (toMatrix a))

    -- | Transposition is an involution
    prop_transpose_involution :: (Eq a, Num a, Show a) => SPL a -> Property
    prop_transpose_involution a = toMatrix (transpose (transpose a)) === toMatrix a
