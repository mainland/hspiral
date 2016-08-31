{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Backend.C
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C (
    evalCg,
    codegen
  ) where

import Prelude hiding ((!!))

import Control.Monad (when)
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Spiral.Backend.C.Array
import Spiral.Backend.C.Assign
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Mapping
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Reduction
import Spiral.Backend.C.Slice
import Spiral.Backend.C.Types
import Spiral.Backend.C.Util
import Spiral.Backend.C.Virtual
import Spiral.Exp
import Spiral.Monad (MonadCg)
import Spiral.SPL
import Spiral.Trace

-- | Generate code for an SPL transform.
codegen :: forall a m .
           ( Num a
           , Num (Exp a)
           , Num (CExp a)
           , ToCExp (Exp a) a
           , ToCType a
           , ToCType (Array D DIM1 a)
           , ToCType (Array D DIM2 a)
           , CTemp a (CExp a)
           , CAssign (CExp a) (CExp a)
           , MCArray C DIM1 a
           , CArray C DIM2 a
           , MonadCg m
           )
        => String
        -> Matrix SPL (Exp a)
        -> Cg m ()
codegen name a = do
    traceCg $ text "Compiling:" </> ppr a
    cgTransform name (extent a) $ \x -> cgSPL a x
  where
    cgSPL :: forall r1 r2 . (MCArray r1 DIM1 a, MCArray r2 DIM1 a)
          => Matrix SPL (Exp a)
          -> Vector r1 (CExp a)
          -> Vector r2 (CExp a)
          -> Cg m ()
    cgSPL a@E{} y x = do
        appendComment $ ppr a
        cgMVProd a x >>= compute y

    cgSPL I{} y x =
        compute y $ cdelay x

    cgSPL (L mn n) y x =
        compute y $ cbackpermute (lperm (fromIntegral mn) (fromIntegral n)) x
      where
        lperm :: forall m . MonadCg m => Int -> Int -> CExp Int -> Cg m (CExp Int)
        lperm mn n i = do
            cin <- cacheCExp $ i*cn
            (+) <$> cacheCExp (cin `mod` cmn) <*> cacheCExp (cin `div` cmn)
          where
            cmn = fromIntegral mn
            cn  = fromIntegral n

    cgSPL e@(B K (I m) a) y x = do
        when (n' /= n) $
            faildoc $ text "Non-square matrix in second argument of ⊗:" </> ppr e
        appendComment $ ppr e
        cgFor 0 m $ \i ->
          cgSPL a (slice y (i*toCExp n) 1 n) (slice x (i*toCExp n) 1 n)
      where
        Z :. n :. n' = extent a

    cgSPL e@(B K a (I n)) y x = do
        when (m' /= m) $
            faildoc $ text "Non-square matrix in first argument of ⊗:" </> ppr e
        appendComment $ ppr e
        cgFor 0 m $ \i ->
          cgSPL a (slice y i n m) (slice x i n m)
      where
       Z :. m :. m' = extent a

    cgSPL e@(B DS a b) y x = do
        when (m' /= m) $
            faildoc $ text "Non-square matrix in first argument of ⊕:" </> ppr e
        when (n' /= n) $
            faildoc $ text "Non-square matrix in first argument of ⊕:" </> ppr e
        appendComment $ ppr e
        cgSPL a (slice y 0 1 m)          (slice x 0 1 m)
        cgSPL b (slice y (toCExp m) 1 n) (slice x (toCExp m) 1 n)
      where
        Z :. m :. m' = extent a
        Z :. n :. n' = extent b

    cgSPL e@(B P a b) y x = do
        when (n' /= n) $
            faildoc $ text "Mismatched dimensions in arguments to ×:" </> ppr e
        appendComment $ ppr e
        shouldUnroll n >>= go
      where
        Z :. _m :.  n = extent a
        Z :. n' :. _p = extent b

        go True = do
            t <- newVirtual (ix1 n) 0
            cgSPL b t x
            cgSPL a y t

        go _ = do
            t <- cgTemp (fromFunction (ix1 n) (const (undefined :: a)))
            cgSPL b t x
            cgSPL a y t

    cgSPL a y x = do
        traceCg $ text "Falling back to default compilation path:" </> ppr a
        cgMVProd a x >>= compute y

cbackpermute :: forall r a . CArray r DIM1 a
             => (forall m . MonadCg m => CExp Int -> Cg m (CExp Int))
             -> Vector r (CExp a)
             -> Vector CD (CExp a)
cbackpermute f v = fromCFunction (extent v) g
  where
    g :: MonadCg m => CShapeOf DIM1 -> Cg m (CExp a)
    g (Z :. ci) = do
        ci' <- f ci
        cindex v (Z :. ci')

-- | Set up code generator to compile a transform. The continuation is
-- called with the input vector and output vector.
cgTransform :: forall a m .
               ( MonadCg m
               , ToCType a
               , ToCType (Array D DIM1 a)
               , CTemp a (CExp a)
               , MCArray C DIM1 a
               )
            => String                -- ^ The name of the transform
            -> DIM2                  -- ^ The dimensions of the transform
            -> (Vector C (CExp a) -> Vector C (CExp a) -> Cg m ()) -- ^ The body of the transform
            -> Cg m ()
cgTransform name (Z :. m :. n) k = do
   appendTopDef [cedecl|$esc:("#include <complex.h>")|]
   cvin     <- cgVar "in"
   cvout    <- cgVar "out"
   let cin  =  C (ix1 n) [cexp|$id:cvin|]
   let cout =  C (ix1 m) [cexp|$id:cvout|]
   items <- inNewBlock_ $
            k cout cin
   appendTopFunDef [cedecl|
void $id:name(const restrict $ty:(toCType (fakeArray cin)) $id:cvin,
              restrict $ty:(toCType (fakeArray cout)) $id:cvout)
{
$items:items
}|]

fakeArray :: (Shape sh, IsArray r sh (CExp a))
          => Array r sh (CExp a)
          -> Array D sh a
fakeArray x = fromFunction (extent x) (const (undefined :: a))

-- | Cache a matrix. This generates code for the entire matrix, but the
-- generated code is only used when we index into the matrix
-- symbolically---otherwise we use the elements of the source matrix directly.
cgMatrix :: forall r a .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , ToCType (Array D DIM2 a)
            , IndexedArray r DIM2 (Exp a)
            , CArray C DIM2 a
            )
         => Matrix r (Exp a)
         -> Matrix CD (CExp a)
cgMatrix a = fromCFunction sh cidx
  where
    sh :: DIM2
    sh = extent a

    cidx :: forall m . MonadCg m => CShapeOf DIM2 -> Cg m (CExp a)
    cidx (Z :. CInt i :. CInt j) =
        return $ toCExp (index a (ix2 i j))

    cidx ix = do
      ce <- cacheConst matInit [cty|static const $ty:ctau |]
      let m :: Matrix C (CExp a)
          m = C sh ce
      cindex m ix

    b :: Matrix M (CExp a)
    b = fmap toCExp (manifest a)

    ess :: [[CExp a]]
    ess = toLists b

    cgRow :: [CExp a] -> C.Initializer
    cgRow es = [cinit|{ $inits:(map toInitializer es) }|]

    matInit :: C.Initializer
    matInit = [cinit|{ $inits:(map cgRow ess) }|]

    ctau :: C.Type
    ctau = toCType (fromFunction sh (const (undefined :: a)))

-- | Compile a matrix-vector product @y = A*x@.
cgMVProd :: forall r1 r2 a m .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , ToCType (Array D DIM2 a)
            , CTemp a (CExp a)
            , CAssign (CExp a) (CExp a)
            , IndexedArray r1 DIM2 (Exp a)
            , CArray       r2 DIM1 a
            , CArray C DIM2 a
            , MonadCg m
            )
         => Matrix r1 (Exp a)         -- ^ The matrix @A@
         -> Vector r2 (CExp a)        -- ^ The vector @x@
         -> Cg m (Vector CD (CExp a)) -- ^ The vector @y@
cgMVProd a x = do
    when (n' /= n) $
      faildoc $ text "cgMVProd: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
    return $ fromCFunction (Z :. m) f
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    a' :: Matrix CD (CExp a)
    a' = cgMatrix a

    f :: forall m . MonadCg m => CShapeOf DIM1 -> Cg m (CExp a)
    f (Z :. ci) =
        cindex (sumP $ x *^ ai) Z
      where
        ai = crow a' ci

-- | Extract a row of a C array.
crow :: forall r a . (CTemp a (CExp a), CArray r DIM2 a)
     => Matrix r (CExp a)
     -> CExp Int
     -> Vector CD (CExp a)
crow a ci = fromCFunction (Z :. n) cidx'
  where
    cidx :: forall m . MonadCg m => CShapeOf DIM2 -> Cg m (CExp a)
    (Z :. _m :. n, cidx) = toCFunction (cdelay a)

    cidx' :: MonadCg m => CShapeOf DIM1 -> Cg m (CExp a)
    cidx' (Z :. cj) = cidx (Z :. ci :. cj)
