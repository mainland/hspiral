{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Backend.C
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C (
    evalCg,
    codegen
  ) where

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
import Spiral.Util.Lift
import Spiral.Util.Uniq

-- | Codegen's representation of a vector with begin, stride, and end.
data CVec = CVec CExp Int Int Int

codegen :: forall m . MonadCg m
        => String
        -> Matrix SPL (Exp (Complex Double))
        -> Cg m ()
codegen name e =
    cgTransform name (extent e) $ \vin vout ->
      go vin vout e
  where
    (Z :. m :. n) = extent e

    go :: CVec -> CVec -> Matrix SPL (Exp (Complex Double)) -> Cg m ()
    go vin vout mat =
        cgFor 0 m $ \ci -> do
          cout <- cgVIdx vout ci
          cgAssign cout 0
          cgFor 0 n $ \cj -> do
            cin <- cgVIdx vin cj
            cij <- cgIdx mat (ci, cj)
            cgAssign cout (cout + cin * cij)

-- | Set up code generator to compile a transform. The continuation is
-- called with the input vector and output vector.
cgTransform :: MonadCg m
            => String                    -- ^ The name of the transform
            -> DIM2                      -- ^ The dimensions of the transform
            -> (CVec -> CVec -> Cg m ()) -- ^ The body of the transform
            -> Cg m ()
cgTransform name (Z :. m :. n) k = do
   appendTopDef [cedecl|$esc:("#include <complex.h>")|]
   cin   <- cvar "in"
   cout  <- cvar "out"
   items <- inNewBlock_ $
            k (CVec (CExp [cexp|$id:cin|])  0 1 n)
              (CVec (CExp [cexp|$id:cout|]) 0 1 m)
   appendTopFunDef [cedecl|
void $id:name(restrict double _Complex $id:cout[static $int:m],
         restrict const double _Complex $id:cin[static $int:n])
{
$items:items
}|]

-- | Generate code to index into a matrix.
cgIdx :: MonadCg m
      => Matrix SPL (Exp (Complex Double)) -- ^ Matrix
      -> (CExp, CExp)                  -- ^ Index
      -> Cg m CExp
cgIdx e (CInt i, CInt j) =
   cgExp $ e ! ix2 (fromInteger i) (fromInteger j)

cgIdx e (ci, cj) = do
   cmat <- cgMatrix $ manifest e
   return $ CExp [cexp|$cmat[$ci][$cj]|]

-- | Generate code to index into a 'CVec m'.
cgVIdx :: MonadCg m
       => CVec      -- ^ Vector
       -> CExp      -- ^ Index
       -> Cg m CExp
cgVIdx (CVec cv off stride _end) ci =
    return $ CExp [cexp|$cv[$(fromIntegral off + ci*fromIntegral stride)]|]

-- | Compile an 'Exp a'
cgExp :: forall a m . MonadCg m => Exp a -> Cg m CExp
cgExp (IntC x)      = return $ CInt x
cgExp (DoubleC x)   = return $ CDouble (toRational x)
cgExp (RationalC x) = return $ CDouble x

cgExp (ComplexC e1 e2) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    go ce1 ce2
  where
    go :: CExp -> CExp -> Cg m CExp
    go ce1 ce2
      | isZero e1 && isOne e2    = return $ CExp [cexp|I|]
      | isZero e1 && isNegOne e2 = return $ CExp [cexp|-I|]
      | isZero e1                = return $ CExp [cexp|$ce2 * I|]
      | isZero e2                = return ce1
      | otherwise                = return $ CExp [cexp|$ce1 + $ce2 * I|]

cgExp e@RouC{} = cgExp (toComplex e)

-- | Compile an assignment.
cgAssign :: MonadCg m => CExp -> CExp -> Cg m ()
cgAssign ce1 ce2 = appendStm [cstm|$ce1 = $ce2;|]

-- | Generate code for a loop with the given start and end.
cgFor :: MonadCg m
      => Int               -- ^ Initial value
      -> Int               -- ^ Upper bound (non-inclusive)
      -> (CExp -> Cg m ()) -- ^ Loop body
      -> Cg m ()
cgFor lo hi k = do
    maxun <- asksConfig maxUnroll
    if hi - lo <= maxun
      then mapM_ k [CInt (fromIntegral i) | i <- [lo..hi-1::Int]]
      else do
        ci    <- cvar "i"
        items <- inNewBlock_ $ k (CExp [cexp|$id:ci|])
        appendStm [cstm|for (int $id:ci = $int:lo; $id:ci < $int:hi; ++$id:ci) $stm:(toStm items)|]

cvar :: MonadUnique m => String -> Cg m C.Id
cvar = gensym

cgMatrix :: forall m . MonadCg m
         => Matrix M (Exp (Complex Double))
         -> Cg m CExp
cgMatrix mat = do
    maybe_ce <- lookupMatrix mat
    case maybe_ce of
      Just ce -> return ce
      Nothing -> do ce <- cgMat
                    cacheMatrix mat ce
                    return ce
  where
    Z :. m :. n = extent mat
    ess = toLists mat

    cgRow :: [Exp (Complex Double)] -> Cg m CExp
    cgRow es = do
        ces <- mapM cgExp es
        return $ CInit [cinit|{ $inits:(map toInitializer ces) }|]

    cgMat :: Cg m CExp
    cgMat = do
      cmat  <- cvar "mat"
      crows <- mapM cgRow ess
      appendTopDecl [cdecl|static const double _Complex $id:cmat[$int:m][$int:n] = { $inits:(map toInitializer crows) };|]
      return $ CExp [cexp|$id:cmat|]
