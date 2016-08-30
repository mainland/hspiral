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
-- Module      :  Spiral.Backend.C.Array
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Array (
    ToCShape(..),

    IsCArray(..),
    Array(..),

    CIndex(..),

    C,

    CD,
    fromCFunction,
    toCFunction,
    cdelay
  ) where

import Prelude hiding ((!!))

import Control.Monad ((>=>))
import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Spiral.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
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

instance ToCShape Z where
    type CShapeOf Z = Z

    toCShape sh = sh

instance ToCShape sh => ToCShape (sh :. Int) where
    type CShapeOf (sh :. Int) = CShapeOf sh :. CExp Int

    toCShape (sh :. i) = toCShape sh :. CInt i

class (ToCShape sh, ToCType e, IsArray r sh (CExp e)) => IsCArray r sh e where
    cindex :: (Shape sh, MonadCg m)
           => Array r sh (CExp e)
           -> CShapeOf sh
           -> Cg m (CExp e)

    compute :: forall m . MonadCg m
            => C.Exp
            -> Array r sh (CExp e)
            -> Cg m ()
    compute cdst a =
        mapM_ assign [0..size sh-1]
      where
        sh :: sh
        sh = extent a

        assign :: Int -> Cg m ()
        assign i = do
            csrc <- cindex a (toCShape ix)
            appendStm [cstm|$(foldr cidx cdst (listOfShape ix)) = $csrc;|]
          where
            ix :: sh
            ix = fromIndex sh i

            cidx :: Int -> C.Exp -> C.Exp
            cidx ci ce = [cexp|$ce[$int:ci]|]

-- | Array indexing that may require code generation.
class CIndex r sh ix e where
    (!!) :: MonadCg m => Array r sh (CExp e) -> ix -> Cg m (CExp e)

instance IsCArray r DIM1 e => CIndex r DIM1 Int e where
    a !! i = cindex a (Z :. CInt i)

instance IsCArray r DIM2 e => CIndex r DIM2 (Int, Int) e where
    a !! (i, j) = cindex a (Z :. CInt i :. CInt j)

instance IsCArray r DIM1 e => CIndex r DIM1 (CExp Int) e where
    a !! ci = cindex a (Z :. ci)

instance IsCArray r DIM2 e => CIndex r DIM2 (CExp Int, CExp Int) e where
    a !! (ci, cj) = cindex a (Z :. ci :. cj)

-- | Type tag for a manifest C array.
data C

instance IsArray C sh (CExp e) where
    -- | A manifest C array. An array with the type tag 'C' is guaranteed to
    -- have contiguously-allocated storage, so it can be efficiently indexed.
    data Array C sh (CExp e) = C sh (CExp e)

    extent (C sh _) = sh

instance IndexedArray C sh (CExp e) where
    index (C _ ce) i = foldr cidx ce (listOfShape i)
      where
        cidx :: Int -> CExp a -> CExp a
        cidx ci ce = CExp [cexp|$ce[$int:ci]|]

instance (ToCShape sh, ToCType e) => IsCArray C sh e where
    cindex (C _ ce) i = return $ foldr cidx ce (listOfCShape i)
      where
        cidx :: CExp Int -> CExp e -> CExp e
        cidx ci ce = CExp [cexp|$ce[$ci]|]

-- | Type tag for a delayed C array.
data CD

instance ToCShape sh => IsArray CD sh (CExp e) where
    -- | A delayed C array.
    data Array CD sh (CExp e) = CD sh (forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))

    extent (CD sh _) = sh

instance (ToCShape sh, ToCType e) => IsCArray CD sh e where
    cindex (CD _ f) = f

-- | Create a delayed array from a function mapping indices to elements.
fromCFunction :: sh
              -> (forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))
              -> Array CD sh (CExp e)
fromCFunction = CD

toCFunction :: (Shape sh, IsCArray r sh e)
            => Array r sh (CExp e)
            -> (sh, forall m . MonadCg m => CShapeOf sh -> Cg m (CExp e))
toCFunction a =
    case cdelay a of
      CD sh f -> (sh, f)

cdelay :: (Shape sh, IsCArray r sh e)
       => Array r sh (CExp e)
       -> Array CD sh (CExp e)
cdelay a = CD (extent a) (cindex a >=> cacheCExp)
