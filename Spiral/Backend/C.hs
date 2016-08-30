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
import Spiral.Backend.C.Types
import Spiral.Backend.C.Util
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
           , CAssign (CExp a)
           , MonadCg m
           )
        => String
        -> Matrix SPL (Exp a)
        -> Cg m ()
codegen name a = do
    traceCg $ text "Compiling:" </> ppr a
    cgTransform name (extent a) $ \x y -> cgSPL a x y
  where
    cgSPL :: Matrix SPL (Exp a)
          -> Vector C (CExp a)
          -> Vector C (CExp a)
          -> Cg m ()
    cgSPL a@E{} x y =
        cgMVProd a x y

    cgSPL I{} x y =
        y .:=. x

    cgSPL (L mn n) x y =
        cgFor 0 m $ \i ->
          cgFor 0 n $ \j ->
              y ! (i + j * toCExp m) .:=. x ! (i * toCExp n + j)
      where
        m = mn `quot` n

    cgSPL e@(B K (I m) a) x y = do
        when (n' /= n) $
            faildoc $ text "Non-square matrix in second argument of ⊗:" </> ppr e
        cgFor 0 m $ \i ->
          cgMVProd a (slice x (i*toCExp n) 1 n) (slice y (i*toCExp n) 1 n)
      where
        Z :. n :. n' = extent a

    cgSPL e@(B K a (I n)) x y = do
        when (m' /= m) $
            faildoc $ text "Non-square matrix in first argument of ⊗:" </> ppr e
        cgFor 0 m $ \i ->
          cgMVProd a (slice x i n m) (slice y i n m)
      where
       Z :. m :. m' = extent a

    cgSPL e@(B DS a b) x y = do
        when (m' /= m) $
            faildoc $ text "Non-square matrix in first argument of ⊕:" </> ppr e
        when (n' /= n) $
            faildoc $ text "Non-square matrix in first argument of ⊕:" </> ppr e
        cgMVProd a (slice x 0 1 m) (slice y 0 1 m)
        cgMVProd b (slice x (toCExp m) 1 n) (slice y (toCExp m) 1 n)
      where
        Z :. m :. m' = extent a
        Z :. n :. n' = extent b

    cgSPL e@(B P a b) x y = do
        when (n' /= n) $
            faildoc $ text "Mismatched dimensions in arguments to ×:" </> ppr e
        CExp ce <- cgTemp (fromFunction (ix1 n) (const (0 :: a)))
        let t :: Vector C (CExp a)
            t = C (ix1 n) (CExp ce)
        cgSPL b x t
        cgSPL a t y
      where
        Z :. _m :.  n = extent a
        Z :. n' :. _p = extent b

    cgSPL a x y = do
        traceCg $ text "Falling back to default compilation path:" </> ppr a
        cgMVProd a x y

-- | Set up code generator to compile a transform. The continuation is
-- called with the input vector and output vector.
cgTransform :: forall a m .
               ( MonadCg m
               , ToCType a
               )
            => String                -- ^ The name of the transform
            -> DIM2                  -- ^ The dimensions of the transform
            -> (Vector C (CExp a) ->
                Vector C (CExp a) ->
                Cg m ())             -- ^ The body of the transform
            -> Cg m ()
cgTransform name (Z :. m :. n) k = do
   appendTopDef [cedecl|$esc:("#include <complex.h>")|]
   cin   <- cgVar "in"
   cout  <- cgVar "out"
   items <- inNewBlock_ $
            k (C (ix1 n) (CExp [cexp|$id:cin|]))
              (C (ix1 m) (CExp [cexp|$id:cout|]))
   appendTopFunDef [cedecl|
void $id:name(restrict $ty:ctau $id:cout[static $int:m],
              restrict $ty:ctau $id:cin[static $int:n])
{
$items:items
}|]
  where
    ctau :: C.Type
    ctau = toCType (undefined :: a)

-- | Cache a matrix. This generates code for the entire matrix, but the
-- generated code is only used when we index into the matrix
-- symbolically---otherwise we use the elements of the source matrix directly.
cgMatrix :: forall r a m .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , IndexedArray r DIM2 (Exp a)
            , MonadCg m)
         => Matrix r (Exp a)
         -> Cg m (Matrix CD (CExp a))
cgMatrix a = do
    ce <- cacheConst matInit [cty|static const $ty:ctau [$int:m][$int:n]|]
    return $ fromCFunction sh (cidx ce)
  where
    sh :: DIM2
    sh@(Z :. m :. n) = extent a

    cidx :: C.Exp -> CShape DIM2 -> CExp a
    cidx _ce (Z :. CInt i :. CInt j) = toCExp (index a (ix2 i j))
    cidx ce  (Z :. ci :. cj)         = CExp [cexp|$ce[$ci][$cj]|]

    b :: Matrix M (CExp a)
    b = fmap toCExp (manifest a)

    ess :: [[CExp a]]
    ess = toLists b

    cgRow :: [CExp a] -> C.Initializer
    cgRow es = [cinit|{ $inits:(map toInitializer es) }|]

    matInit :: C.Initializer
    matInit = [cinit|{ $inits:(map cgRow ess) }|]

    ctau :: C.Type
    ctau = toCType (undefined :: a)

-- | Compile a matrix-vector product @y = A*x@.
cgMVProd :: forall r1 r2 r3 a m .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , CAssign (CExp a)
            , IndexedArray  r1 DIM2 (Exp a)
            , IndexedArray  r2 DIM1 (CExp a)
            , IsCArray r2 DIM1 a
            , IsArray  r3 DIM1 (CExp a)
            , IsCArray r3 DIM1 a
            , Index r2 DIM1 (CExp Int) (CExp a)
            , Index r3 DIM1 (CExp Int) (CExp a)
            , CIndex r3 DIM1 (CExp Int) a
            , MonadCg m
            )
         => Matrix r1 (Exp a)  -- ^ The matrix @A@
         -> Vector r2 (CExp a) -- ^ The vector @x@
         -> Vector r3 (CExp a) -- ^ The vector @y@
         -> Cg m ()
cgMVProd a x y = do
    when (n' /= n) $
      faildoc $ text "cgMVProd: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
    when (m' /= m) $
      faildoc $ text "cgMVProd: mismatched dimensions in output. Expected" <+> ppr m <+> text "but got" <+> ppr m'
    a' <- cgMatrix a
    cgFor 0 m $ \i -> do
      let ai = crow a' i
      let yi = y ! i
      let v1 = x *^ ai
      let v2 = sumP v1
      compute [cexp|$yi|] v2
  where
    Z :. m'     = extent y
    Z :. n'     = extent x
    Z :. m :. n = extent a

-- | Extract a row of a C array.
crow :: forall r a . IsCArray r DIM2 a => Matrix r (CExp a) -> CExp Int -> Vector CD (CExp a)
crow a ci = fromCFunctions (Z :. n) cidx' cidxm'
  where
    cidx :: CShape DIM2 -> CExp a
    cidxm :: forall m . MonadCg m => CShape DIM2 -> Cg m (CExp a)
    (Z :. _m :. n, cidx, cidxm) = toCFunctions (cdelay a)

    cidx' :: CShape DIM1 -> CExp a
    cidx' (Z :. cj) = cidx (Z :. ci :. cj)

    cidxm' :: MonadCg m => CShape DIM1 -> Cg m (CExp a)
    cidxm' (Z :. cj) = cidxm (Z :. ci :. cj)

-- | Type tag for a vector slice.
data S

-- | A vector slice in @begin:stride:len@ form. @begin@ is symbolic, where as
-- @stride@ and @len@ are statically known. Note that the end of the slice is
-- @begin + len - 1@.
instance IsArray S DIM1 a where
    data Array S DIM1 a where
        S :: (Index r DIM1 (CExp Int) a, CIndex r DIM1 (CExp Int) a)
          => Array r DIM1 a
          -> CExp Int
          -> Int
          -> Int
          -> Array S DIM1 a

    extent (S _ _b _s len) = Z :. len

instance IndexedArray S DIM1 a where
    index (S a b s _len) (Z :. i) = a ! (b + fromIntegral (i*s))

instance Index S DIM1 (CExp Int) a where
    (!) (S a b s _e) ci = a ! (b + ci * fromIntegral s)

instance Pretty (Array S DIM1 a) where
    ppr (S _ b s e) = brackets $ colonsep [text "...", ppr b, ppr s, ppr e]
      where
        colonsep :: [Doc] -> Doc
        colonsep = align . sep . punctuate colon

instance ToCType e => IsCArray S (Z :. Int) e where
    cindex (S a b s _len) (Z :. ci) = a ! (b + ci * fromIntegral s)

instance ToCType e => CIndex S DIM1 (CExp Int) e where
    a !! ci = cindexM a (Z :. ci)

slice :: ( IsArray r DIM1 a
         , Index  r DIM1 (CExp Int) a
         , CIndex r DIM1 (CExp Int) a
         )
      => Array r DIM1 a
      -> CExp Int
      -> Int
      -> Int
      -> Array S DIM1 a
slice = S
