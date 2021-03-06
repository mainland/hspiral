{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    smartConstructorTests,
    transposeTest,
    inverseTest,
    mXvTest
  ) where

import Control.Monad (replicateM)
import Data.Ratio
import qualified Data.Vector as V
import Test.HUnit ((@?=))
import Test.Hspec
import Test.QuickCheck

import Spiral.Array (M,
                     Matrix)
import qualified Spiral.Array as A
import qualified Spiral.Array.Operators.Matrix as A
import Spiral.NumberTheory (factors)
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
    smartConstructorTests
    transposeTest
    inverseTest
    mXvTest

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
    arbitrary = sized $ \k ->
        if k == 0
          then pure $ J 1
          else oneof [ do (m, n) <- elements $ (1,k) : factors k
                          pure $ L (m*n) n
                     , pure $ J k
                     ]

instance (Num a, Arbitrary a) => Arbitrary (A.Vector A.M a) where
    arbitrary = sized $ \n -> do
        xs <- replicateM n arbitrary
        return $ A.M (ix1 n) (V.fromList xs)

    shrink (A.M _ v)
      | n > 1     = [A.M (ix1 (n-1)) (V.init v), A.M (ix1 (n-1)) (V.tail v)]
      | otherwise = []
      where
        n = V.length v

instance (Num a, Arbitrary a) => Arbitrary (SPL a) where
    arbitrary = sized $ \n ->
        oneof [ matrixGen n n
              , requireSize 1 $ pure $ I n
              , piGen
              , T <$> resize n arbitrary
              , requireSize 1 $ Diag . V.fromList <$> replicateM n arbitrary
              , requireSize 1 $ Circ . V.fromList <$> replicateM n arbitrary
              , requireSize 1 $ Skew . V.fromList <$> replicateM n arbitrary
              , requireSize 1 $ Toep . V.fromList <$> replicateM (2*n-1) arbitrary
              , aboveGen
              , besideGen
              , kronGen
              , dsumGen
              ]
      where
        emptyMatrix :: SPL a
        emptyMatrix = matrix $ A.M (ix2 0 0) V.empty

        matrixGen :: Arbitrary a => Int -> Int -> Gen (SPL a)
        matrixGen r c
          | r == 0 || c == 0 = pure emptyMatrix
          | otherwise        = fromLists <$> replicateM r (replicateM c arbitrary)

        requireSize :: Int -> Gen (SPL a) -> Gen (SPL a)
        requireSize n g = sized $ \n' -> if n >= n' then matrixGen n' n' else g

        piGen :: Gen (SPL a)
        piGen = requireSize 1 $ Pi <$> arbitrary

        aboveGen :: Gen (SPL a)
        aboveGen = requireSize 2 $ sized $ \n ->
            let (q, r) = quotRem n 2
            in
              Above <$> matrixGen q n <*> matrixGen (q + r) n

        besideGen :: Gen (SPL a)
        besideGen = requireSize 2 $ sized $ \n ->
            let (q, r) = quotRem n 2
            in
              Beside <$> matrixGen n q <*> matrixGen n (q + r)

        kronGen :: Gen (SPL a)
        kronGen = requireSize 2 $ sized $ \k -> do
            (m, n) <- elements $ (1,k) : factors k
            oneof [ Kron <$> pure (I m) <*> resize n arbitrary
                  , Kron <$> resize m arbitrary <*> pure (I n)
                  , Kron <$> resize m arbitrary <*> resize n arbitrary
                  ]

        dsumGen :: Gen (SPL a)
        dsumGen = requireSize 2 $ sized $ \k -> do
            m     <- elements [1..k-1]
            let n =  k-m
            DSum <$> resize m arbitrary <*> resize n arbitrary

    shrink a0@E{}
        | m > 1 && n > 1 = [ matrix $ A.submatrix 0 (m-1) 0 (n-1) a
                           , matrix $ A.submatrix 0 (m-1) 1 (n-1) a
                           , matrix $ A.submatrix 1 (m-1) 0 (n-1) a
                           , matrix $ A.submatrix 1 (m-1) 1 (n-1) a
                           ]
      where
        a = toMatrix a0

        Z :. m :. n = extent a0

    shrink (I n) = [I (n-1)]

    shrink (T a) = map T (shrink a)

    shrink (Diag xs)
      | V.length xs > 2 = map (Diag . V.fromList) (shrink (V.toList xs))

    shrink (Circ xs)
      | V.length xs > 2 = map (Circ . V.fromList) (shrink (V.toList xs))

    shrink (Skew xs)
      | V.length xs > 2 = map (Skew . V.fromList) (shrink (V.toList xs))

    shrink (Toep xs)
      | V.length xs > 3 = map (Toep . V.fromList) (concatMap shrink (shrink (V.toList xs)))

    shrink a
        | m > 1 && n > 1 = shrink (matrix (toMatrix a))
      where
        Z :. m :. n = extent a

    shrink _ = []

smartConstructorTests :: Spec
smartConstructorTests = describe "Smart constructor tests" $ do
    smartKroneckerTest
    smartDSumTest
    smartProdTest

data KronTest a = KronTest (SPL a) (SPL a)
  deriving (Show)

instance (Num a, Arbitrary a) => Arbitrary (KronTest a) where
    arbitrary = sized $ \k ->
        if k == 0
          then return $ KronTest (I 1) (I 1)
          else do (m, n) <- elements $ (1,k) : factors k
                  KronTest <$> resize m arbitrary <*> resize n arbitrary

smartKroneckerTest :: Spec
smartKroneckerTest = it "Kronecker" $
    property (prop_kronecker_smart :: KronTest Int -> Property)
  where
    prop_kronecker_smart :: (Eq a, Num a, Show a) => KronTest a -> Property
    prop_kronecker_smart (KronTest a b) =
      toMatrix (a ⊗ b) === toMatrix (Kron a b)

smartDSumTest :: Spec
smartDSumTest = it "Direct sum" $
    property (prop_dsum_smart :: SPL Int -> SPL Int -> Property)
  where
    prop_dsum_smart :: (Eq a, Num a, Show a) => SPL a -> SPL a -> Property
    prop_dsum_smart a b =
      toMatrix (a ⊕ b) === toMatrix (DSum a b)

smartProdTest :: Spec
smartProdTest = it "Product" $
    property (prop_prod_smart :: SPL Int -> SPL Int -> Property)
  where
    prop_prod_smart :: (Eq a, Num a, Show a) => SPL a -> SPL a -> Property
    prop_prod_smart a b =
      toMatrix (a × b) === toMatrix (Prod a b)

-- | Test group to verify matrix transposition of different matrix shapes
transposeTest :: Spec
transposeTest = describe "Matrix transpose" $ do
    it "Transpose of a permutation is inverse of permutation" $
        property prop_transpose_permutation
    it "Transpose commutes" $
        property (prop_transpose_commutes :: SPL Int -> Property)
    it "Transpose is an involution" $
        property (prop_transpose_involution :: SPL Int -> Property)
  where
    -- | Transpostion of a permutation is the inverse of the permutation
    prop_transpose_permutation :: Permutation -> Property
    prop_transpose_permutation p = toMatrix (transpose (permute p :: SPL Int)) === toMatrix (backpermute p)

    -- | Transpostion commutes with matrix conversion
    prop_transpose_commutes :: (Eq a, Num a, Show a) => SPL a -> Property
    prop_transpose_commutes a = toMatrix (transpose a) === A.manifest (A.transpose (toMatrix a))

    -- | Transposition is an involution
    prop_transpose_involution :: (Eq a, Num a, Show a) => SPL a -> Property
    prop_transpose_involution a = toMatrix (transpose (transpose a)) === toMatrix a

newtype SmallRational = SR Rational
  deriving (Eq, Ord, Show, Num, Fractional)

instance Arbitrary SmallRational where
    arbitrary = do n :: Integer <- arbitrary
                   return $ SR $ (n `mod` 20) % 1

-- | Test group to verify matrix inversion
inverseTest :: Spec
inverseTest = describe "Matrix inverse" $ do
    it "Inverse of a permutation is inverse of permutation" $
        property prop_inverse_permutation
    it "Inverse commutes" $
        property (prop_inverse_commutes :: SPL SmallRational -> Property)
    it "Inverse is an involution" $
        property (prop_inverse_involution :: SPL SmallRational -> Property)
  where
    -- | Inverse of a permutation is the inverse of the permutation
    prop_inverse_permutation :: Permutation -> Property
    prop_inverse_permutation p = toMatrix (inverse (permute p :: SPL Rational)) === toMatrix (backpermute p)

    -- | Inverse commutes with matrix conversion
    prop_inverse_commutes :: (Eq a, Fractional a, Show a) => SPL a -> Property
    prop_inverse_commutes a =
        case A.inverse (toMatrix a) of
          Just a' -> toMatrix (inverse a) === A.manifest a'
          Nothing -> property True

    -- | Inverse is an involution
    prop_inverse_involution :: (Eq a, Fractional a, Show a) => SPL a -> Property
    prop_inverse_involution a =
        case A.inverse (toMatrix a) of
          Just _  -> toMatrix (inverse (inverse a)) === toMatrix a
          Nothing -> property True

data MXV a = MXV (SPL a) (A.Vector M a)
  deriving (Show)

instance (Num a, Show a, Arbitrary a) => Arbitrary (MXV a) where
    arbitrary = sized $ \n -> do
        a <- resize n arbitrary
        let Z :. _ :. m' = extent a
        x <- resize m' arbitrary
        return $ MXV a x

    shrink (MXV a (A.M _ xs))
      | m > 1 && n > 1 = do a' <- shrink a
                            let Z :. _ :. n' = extent a'
                            return $ MXV a' (A.M (ix1 n') (V.take n' xs))
      | otherwise = []
      where
        Z :. m :. n = extent a

-- | Test matrix-vector product
mXvTest :: Spec
mXvTest = describe "Matrix-vector product" $
    it "mXv commutes" $
        property (prop_mXv_commutes :: MXV Int -> Property)
  where
    -- | mXv commutes with matrix conversion
    prop_mXv_commutes :: (Eq a, Num a, Show a) => MXV a -> Property
    prop_mXv_commutes (MXV a x) = A.manifest (a #> x) === A.manifest (toMatrix a A.#> x)
