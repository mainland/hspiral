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
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Spiral.Backend.C.Array
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Array (
    ToCShape(..),

    CArray(..),
    MCArray(..),
    CIndex(..),

    Array(..),

    C,

    CD,
    fromCFunction,
    toCFunction,
    cdelay
  ) where

import Prelude hiding ((!!))

import Control.Monad ((>=>))
import Data.Complex
import Language.C.Quote.C
import qualified Language.C.Quote as C

import Spiral.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Globals
import Spiral.Monad (MonadCg)
import Spiral.Shape

class CShape sh where
    listOfCShape :: sh -> [CExp Int]

instance CShape Z where
    listOfCShape _ = []

instance CShape sh => CShape (sh :. CExp Int) where
    listOfCShape (sh :. ci) = ci : listOfCShape sh

class (Shape sh, CShape (CShapeOf sh)) => ToCShape sh where
    type CShapeOf sh

    toCShape :: sh -> CShapeOf sh

    fromCShape :: CShapeOf sh -> sh

    cgForShape :: MonadCg m
               => sh
               -> (CShapeOf sh -> Cg m ())
               -> Cg m ()

instance ToCShape Z where
    type CShapeOf Z = Z

    toCShape sh = sh

    fromCShape sh = sh

    cgForShape _ k = k Z

instance ToCShape sh => ToCShape (sh :. Int) where
    type CShapeOf (sh :. Int) = CShapeOf sh :. CExp Int

    toCShape (sh :. i) = toCShape sh :. CInt i

    fromCShape (sh :. CInt i) = fromCShape sh :. i
    fromCShape _              = error "fromCShape: bad shape"

    cgForShape (sh :. i) k =
      cgForShape sh $ \csh ->
        cgFor 0 i $ \ci ->
          k (csh :. ci)

class (ToCShape sh, ToCType e, IsArray r sh (CExp e)) => CArray r sh e where
    cindex :: (Shape sh, MonadCg m)
           => Array r sh (CExp e)
           -> CShapeOf sh
           -> Cg m (CExp e)

    compute :: forall r' m . (MonadCg m, CTemp e (CExp e), MCArray r' sh e)
            => Array r' sh (CExp e)
            -> Array r sh (CExp e)
            -> Cg m ()
    compute a b =
        cgForShape (extent b) $ \ix -> do
            x <- cindex b ix >>= lookupCExp
            cwrite a ix x

-- | A 'CArray' with mutable values.
class CArray r sh e => MCArray r sh e where
    cwrite :: (Shape sh, MonadCg m)
           => Array r sh (CExp e)
           -> CShapeOf sh
           -> CExp e
           -> Cg m ()

-- | Array indexing that may require code generation.
class CIndex r sh ix e where
    (!!) :: MonadCg m => Array r sh (CExp e) -> ix -> Cg m (CExp e)

instance CArray r DIM1 e => CIndex r DIM1 Int e where
    a !! i = cindex a (Z :. CInt i)

instance CArray r DIM2 e => CIndex r DIM2 (Int, Int) e where
    a !! (i, j) = cindex a (Z :. CInt i :. CInt j)

instance CArray r DIM1 e => CIndex r DIM1 (CExp Int) e where
    a !! ci = cindex a (Z :. ci)

instance CArray r DIM2 e => CIndex r DIM2 (CExp Int, CExp Int) e where
    a !! (ci, cj) = cindex a (Z :. ci :. cj)

-- | Type tag for a manifest C array.
data C

instance IsArray C sh (CExp e) where
    -- | A manifest C array. An array with the type tag 'C' is guaranteed to
    -- have contiguously-allocated storage, so it can be efficiently indexed.
    data Array C sh (CExp e) = C sh C.Exp

    extent (C sh _) = sh

mkIdx :: forall e . C.ToExp e => C.Exp -> [e] -> C.Exp
mkIdx = foldr cidx
  where
    cidx :: e -> C.Exp -> C.Exp
    cidx ci ce = [cexp|$ce[$ci]|]

mkComplexIdx :: forall e . (Num e, C.ToExp e)
             => C.Exp
             -> [e]
             -> (C.Exp, C.Exp)
mkComplexIdx ce [] =
    ([cexp|$ce[0]|], [cexp|$ce[1]|])

mkComplexIdx ce (ci:cis) =
    (mkIdx ce (2*ci:cis), mkIdx ce (2*ci+1:cis))

instance IndexedArray C sh (CExp Int) where
    index (C _ ce) i = CExp $ mkIdx ce (listOfShape i)

instance IndexedArray C sh (CExp Double) where
    index (C _ ce) i = CExp $ mkIdx ce (listOfShape i)

instance IndexedArray C sh (CExp (Complex Double)) where
    index (C _ ce) i | useComplexType =
        CExp $ mkIdx ce (listOfShape i)

    index (C _ ce) i =
        CComplex (CExp cr) (CExp ci)
      where
        (cr, ci) = mkComplexIdx ce (listOfShape i)

instance ToCShape sh => CArray C sh Int where
    cindex (C _ ce) i = return $ CExp $ mkIdx ce (listOfCShape i)

instance ToCShape sh => CArray C sh Double where
    cindex (C _ ce) i = return $ CExp $ mkIdx ce (listOfCShape i)

instance ToCShape sh => CArray C sh (Complex Double) where
    cindex (C _ ce) i | useComplexType =
        return $ CExp $ mkIdx ce (listOfCShape i)

    cindex (C _ ce) i =
        return $ CComplex (CExp cr) (CExp ci)
      where
        (cr, ci) = mkComplexIdx ce (listOfCShape i)

instance ToCShape sh => MCArray C sh Int where
    cwrite (C _ ce) i ce' =
        appendStm [cstm|$(mkIdx ce (listOfCShape i)) = $ce';|]

instance ToCShape sh => MCArray C sh Double where
    cwrite (C _ ce) i ce' =
        appendStm [cstm|$(mkIdx ce (listOfCShape i)) = $ce';|]

instance ToCShape sh => MCArray C sh (Complex Double) where
    cwrite (C _ ce) i ce' | useComplexType =
        appendStm [cstm|$(mkIdx ce (listOfCShape i)) = $ce';|]

    cwrite (C _ ce) i ce' = do
        appendStm [cstm|$cr = $cr';|]
        appendStm [cstm|$ci = $ci';|]
      where
        (cr, ci)   = mkComplexIdx ce (listOfCShape i)
        (cr', ci') = unComplex ce'

instance ( Shape sh
         , ToCType (Array r sh a)
         , IsArray r sh a
         , IsArray r sh (CExp a)
         )
      => CTemp (Array r sh a) (Array C sh (CExp a)) where
    cgTemp a = do
        ce <- cgRawTemp a
        return $ C (extent a) ce

-- | Type tag for a delayed C array.
data CD

instance ToCShape sh => IsArray CD sh (CExp e) where
    -- | A delayed C array.
    data Array CD sh (CExp e) = CD sh (forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))

    extent (CD sh _) = sh

instance (ToCShape sh, ToCType e) => CArray CD sh e where
    cindex (CD _ f) = f

-- | Create a delayed array from a function mapping indices to elements.
fromCFunction :: sh
              -> (forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))
              -> Array CD sh (CExp e)
fromCFunction = CD

toCFunction :: (Shape sh, CTemp e (CExp e), CArray r sh e)
            => Array r sh (CExp e)
            -> (sh, forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))
toCFunction a =
    case cdelay a of
      CD sh f -> (sh, f)

cdelay :: (Shape sh, CTemp e (CExp e), CArray r sh e)
       => Array r sh (CExp e)
       -> Array CD sh (CExp e)
cdelay a = CD (extent a) (cindex a >=> cacheCExp)
