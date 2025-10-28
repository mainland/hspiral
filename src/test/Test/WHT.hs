{-# LANGUAGE ScopedTypeVariables #-}

module Test.WHT (whtTests) where

import Test.Hspec
import Test.HUnit ((@?=))

import qualified Spiral.Array as A
import Spiral.Array (M, Matrix)
import Spiral.SPL
import Spiral.Exp

import qualified Spiral.Array.Operators.Mapping as A
import qualified Spiral.Array.Operators.Matrix as A

-- Hadamard 2x2
h1 :: Num a => Matrix M a
h1 = A.manifest $ A.matrix [[1, 1], [1, -1]]

hadamard :: Floating a => Int -> Matrix M a
hadamard n 
  | n <= 0          = error "hadamard: n must be positive"
  | n == 0          = A.manifest $ A.matrix [[1]]
  | n == 1          = h1
  | otherwise       = A.manifest $ A.kronecker h1 (hadamard (n - 1))

whtTests :: Spec
whtTests = describe "WHT" $ do
    it "WHT matrix equals hadamard for n=1,2,3,4" $ do
        toMatrix (WHT 1 :: SPL Double) @?= hadamard 1
        toMatrix (WHT 2 :: SPL Double) @?= hadamard 2
        toMatrix (WHT 3 :: SPL Double) @?= hadamard 3
        toMatrix (WHT 4 :: SPL Double) @?= hadamard 4

    it "WHT' × WHT == I_n for n=1,2,3,4" $ do
        toMatrix (WHT' 1 × WHT 1 :: SPL Double) @?= toMatrix (I (2^1) :: SPL Double)
        toMatrix (WHT' 2 × WHT 2 :: SPL Double) @?= toMatrix (I (2^2) :: SPL Double)
        toMatrix (WHT' 3 × WHT 3 :: SPL Double) @?= toMatrix (I (2^3) :: SPL Double)
        toMatrix (WHT' 4 × WHT 4 :: SPL Double) @?= toMatrix (I (2^4) :: SPL Double)
