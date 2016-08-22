{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Spiral.Backend.C
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C (
    evalCg
  ) where

import Data.Complex
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C

import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Util
import qualified Spiral.Cg.Monad as Cg
import Spiral.Cg.Monad (CVec(..),
                        cgExp)
import Spiral.Config
import Spiral.Exp
import Spiral.SPL
import Spiral.Trace
import Spiral.Util.Lift
import Spiral.Util.Uniq

cvar :: MonadUnique m => String -> Cg m C.Id
cvar = gensym

cgMatrix :: forall m . (MonadConfig m, MonadTrace m, MonadUnique m)
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

instance (MonadConfig m, MonadTrace m, MonadUnique m) => Cg.MonadCg (Cg m) where
     type CExp (Cg m) = CExp

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

     cgTemp = fail "cgTemp: Can't generate temp"

     cgIdx e (CInt i, CInt j) =
         cgExp $ e ! ix2 (fromInteger i) (fromInteger j)

     cgIdx e (ci, cj) = do
         cmat <- cgMatrix $ manifest e
         return $ CExp [cexp|$cmat[$ci][$cj]|]

     cgVIdx (CVec cv off stride _end) ci =
         return $ CExp [cexp|$cv[$(fromIntegral off + ci*fromIntegral stride)]|]

     cgAssign ce1 ce2 =
         appendStm [cstm|$ce1 = $ce2;|]

     cgFor lo hi k = do
         maxun <- asksConfig maxUnroll
         if hi - lo <= maxun
           then mapM_ k [CInt (fromIntegral i) | i <- [lo..hi-1::Int]]
           else do
             ci    <- cvar "i"
             items <- inNewBlock_ $ k (CExp [cexp|$id:ci|])
             appendStm [cstm|for (int $id:ci = $int:lo; $id:ci < $int:hi; ++$id:ci) $stm:(toStm items)|]
