{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Complex
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C

import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Util
import Spiral.Config
import Spiral.Exp
import Spiral.Monad (MonadCg)
import Spiral.SPL
import Spiral.Util.Uniq

codegen :: forall a m .
           ( Num (Exp a)
           , Num (CExp a)
           , ToCExp (Exp a) a
           , ToCType a
           , CAssign (CExp a)
           , MonadCg m
           )
        => String
        -> Matrix SPL (Exp a)
        -> Cg m ()
codegen name a = cgTransform name (extent a) $ \x y -> cgMVProd a x y

-- | Set up code generator to compile a transform. The continuation is
-- called with the input vector and output vector.
cgTransform :: forall a m .
               ( MonadCg m
               , ToCType a
               )
            -- | The name of the transform
            => String
            -- | The dimensions of the transform
            -> DIM2
            -- | The body of the transform
            -> (Vector C (CExp a) -> Vector C (CExp a) -> Cg m ())
            -> Cg m ()
cgTransform name (Z :. m :. n) k = do
   appendTopDef [cedecl|$esc:("#include <complex.h>")|]
   cin   <- cvar "in"
   cout  <- cvar "out"
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

-- | Generate code for a loop with the given start and end.
cgFor :: MonadCg m
      => Int                   -- ^ Initial value
      -> Int                   -- ^ Upper bound (non-inclusive)
      -> (CExp Int -> Cg m ()) -- ^ Loop body
      -> Cg m ()
cgFor lo hi k = do
    maxun <- asksConfig maxUnroll
    if hi - lo <= maxun
      then mapM_ k [CInt i | i <- [lo..hi-1::Int]]
      else do
        ci    <- cvar "i"
        items <- inNewBlock_ $ k (CExp [cexp|$id:ci|])
        appendStm [cstm|for (int $id:ci = $int:lo; $id:ci < $int:hi; ++$id:ci) $stm:(toStm items)|]

cvar :: MonadUnique m => String -> Cg m C.Id
cvar = gensym

cgMatrix :: forall r a m .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , IsArray r DIM2 (Exp a)
            , MonadCg m)
         => Matrix r (Exp a)
         -> Cg m (Matrix CC (CExp a))
cgMatrix a = do
    maybe_ce <- lookupConst matInit
    ce       <- case maybe_ce of
                  Just ce -> return ce
                  Nothing -> do cmat <- cvar "mat"
                                appendTopDecl [cdecl|static const $ty:ctau $id:cmat[$int:m][$int:n] = $init:matInit;|]
                                cacheConst matInit [cexp|$id:cmat|]
                                return [cexp|$id:cmat|]
    return $ CC sh (delay (fmap Just b)) (CExp ce)
  where
    sh :: DIM2
    sh@(Z :. m :. n) = extent a

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

-- | Compiled a matrix-vector product
cgMVProd :: forall r1 r2 r3 a m .
            ( Num (Exp a)
            , Num (CExp a)
            , ToCExp (Exp a) a
            , ToCType a
            , CAssign (CExp a)
            , IsArray r1 DIM1 (CExp a)
            , IsArray r2 DIM2 (Exp a)
            , IsArray r3 DIM1 (CExp a)
            , Index r1 DIM1 (CExp Int) (CExp a)
            , Index r3 DIM1 (CExp Int) (CExp a)
            , MonadCg m
            )
         => Matrix r2 (Exp a)
         -> Vector r3 (CExp a)
         -> Vector r1 (CExp a)
         -> Cg m ()
cgMVProd a x y = do
    when (m' /= m || n' /= n) $
        fail "cgMVProd: mismatched dimensions"
    a' <- cgMatrix a
    cgFor 0 m $ \i -> do
      let ai = crow a' i
      let yi = y ! i
      cgZipFold (*) x ai (+) 0 yi
  where
    Z :. m'     = extent y
    Z :. n'     = extent x
    Z :. m :. n = extent a

-- | Extract a row of a cached matrix.
crow :: Matrix CC (CExp a)
     -> CExp Int
     -> Vector CC (CExp a)
crow (CC (Z :. _m :. n) a ce) ci@(CInt i) =
    CC sh (fromFunction sh (\(Z :. j) -> a ! (i, j))) (CExp [cexp|$ce[$ci]|])
  where
    sh = Z :. n

crow (CC (Z :. _m :. n) _a ce) ci =
    CC sh (fromFunction sh (const Nothing)) (CExp [cexp|$ce[$ci]|])
  where
    sh = Z :. n

-- | A combined zip/fold over two vectors. We can't quite separate these into
-- two operations because we would need to be able to index into the result of
-- the zip with a symbolic expression to generate the body of the for loop.
cgZipFold :: ( IsArray r1 (Z :. Int) c
             , Index r1 (Z :. Int) (CExp Int) c
             , Index r2 sh (CExp Int) d
             , Index r2 sh Int d
             , CAssign a
             , MonadCg m
             )
          => (c -> d -> b)
          -> Array r1 (Z :. Int) c
          -> Array r2 sh d
          -> (a -> b -> a)
          -> a
          -> a
          -> Cg m ()
cgZipFold g s t f z y = do
    maxun <- asksConfig maxUnroll
    if n <= maxun
      then y .:=. foldl f z [g (s ! i) (t ! i) | i <- [0..n-1]]
      else do
        y .:=. z
        cgFor 0 n $ \j -> do
          let sj = s ! j
          let tj = t ! j
          y .:=. f y (g sj tj)
  where
    Z :. n = extent s

-- | Compile a value to a C expression.
class ToCExp a b | a -> b where
    toCExp :: a -> CExp b

instance ToCExp (CExp a) a where
    toCExp ce = ce

instance ToCExp (Exp Integer) Int where
    toCExp (IntC x) = CInt (fromIntegral x)

instance ToCExp (Exp Double) Double where
    toCExp (DoubleC x) = CDouble (toRational x)

instance ToCExp (Exp (Complex Double)) (Complex Double) where
    toCExp (ComplexC e1 e2) = CComplex (toCExp e1) (toCExp e2)
    toCExp e@RouC{}         = toCExp (toComplex e)

-- | Compile a value to a C type.
class ToCType a where
    toCType :: a -> C.Type

instance ToCType Int where
    toCType _ = [cty|int|]

instance ToCType Double where
    toCType _ = [cty|double|]

instance ToCType (Complex Double) where
    toCType _ = [cty|double _Complex|]

instance (ToCType a, IsArray r DIM1 a) => ToCType (Array r DIM1 a) where
    toCType a = [cty|$ty:(toCType (undefined :: a))[static $int:n]|]
      where
        Z :. n = extent a

instance (ToCType a, IsArray r DIM2 a) => ToCType (Array r DIM2 a) where
    toCType a = [cty|$ty:(toCType (undefined :: a))[static $int:m][static $int:n]|]
      where
        Z :. m :. n = extent a

class CAssign a where
    -- | Compile an assignment.
    cassign :: MonadCg m => a -> a -> Cg m ()

infix 4 .:=.
(.:=.) :: (CAssign a, MonadCg m) => a -> a -> Cg m ()
(.:=.) = cassign

instance CAssign (CExp Int) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp Double) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

instance CAssign (CExp (Complex Double)) where
    cassign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]
