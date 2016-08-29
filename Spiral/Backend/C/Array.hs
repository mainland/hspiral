{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Spiral.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Monad (MonadCg)
import Spiral.Shape

class ToCShape sh where
    type CShape sh

    toCShape :: sh -> CShape sh

instance ToCShape Z where
    type CShape Z = Z

    toCShape sh = sh

instance ToCShape sh => ToCShape (sh :. Int) where
    type CShape (sh :. Int) = CShape sh :. CExp Int

    toCShape (sh :. i) = toCShape sh :. CInt i

class (Shape sh, ToCType e, IsArray r sh (CExp e)) => IsCArray r sh e where
    -- | Shape polymorphic indexing.
    cindex :: Shape sh
           => Array r sh (CExp e)
           -> CShape sh
           -> CExp e

    cindexM :: (Shape sh, MonadCg m)
            => Array r sh (CExp e)
            -> CShape sh
            -> Cg m (CExp e)
    cindexM a ix = cacheCExp $ cindex a ix

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
            csrc <- lookupCExp $ a ! ix
            appendStm [cstm|$(foldr cidx cdst (listOfShape ix)) = $csrc;|]
          where
            ix :: sh
            ix = fromIndex sh i

            cidx :: Int -> C.Exp -> C.Exp
            cidx ci ce = [cexp|$ce[$int:ci]|]

-- | Array indexing that may require code generation.
class CIndex r sh ix e where
    (!!) :: MonadCg m => Array r sh (CExp e) -> ix -> Cg m (CExp e)

instance IsCArray r (Z :. Int) e => CIndex r (Z :. Int) Int e where
    a !! i = cindexM a (Z :. CInt i)

instance IsCArray r (Z :. Int :. Int) e => CIndex r (Z :. Int :. Int) (Int, Int) e where
    a !! (i, j) = cindexM a (Z :. CInt i :. CInt j)

-- | Type tag for a manifest C array.
data C

instance IsArray C sh (CExp e) where
    -- | A manifest C array. An array with the type tag 'C' is guaranteed to
    -- have contiguously-allocated storage, so it can be efficiently indexed.
    data Array C sh (CExp e) = C sh (CExp e)

    extent (C sh _) = sh

    index (C _ ce) i = foldr cidx ce (listOfShape i)
      where
        cidx :: Int -> CExp a -> CExp a
        cidx ci ce = CExp [cexp|$ce[$int:ci]|]

instance Index C (Z :. Int) (CExp Int) (CExp e) where
    (!) (C _ ce) ci = CExp [cexp|$ce[$ci]|]

instance Index C (Z :. Int :. Int) (CExp Int, CExp Int) (CExp e) where
    (!) (C _ ce) (ci, cj) = CExp [cexp|$ce[$ci][$cj]|]

instance ToCType e => IsCArray C (Z :. Int) e where
    cindex (C _ ce) (Z :. ci) = CExp [cexp|$ce[$ci]|]

instance ToCType e => IsCArray C (Z :. Int :. Int) e where
    cindex (C _ ce) (Z :. ci :. cj) = CExp [cexp|$ce[$ci][$cj]|]

instance CIndex C (Z :. Int) (CExp Int) e where
    a !! i = return $ a ! i

instance CIndex C (Z :. Int :. Int) (CExp Int, CExp Int) e where
    a !! i = return $ a ! i

-- | Type tag for a delayed C array.
data CD

instance ToCShape sh => IsArray CD sh (CExp e) where
    -- | A delayed C array.
    data Array CD sh (CExp e) = CD sh (CShape sh -> CExp e)

    extent (CD sh _) = sh

    index (CD _ f) sh = f (toCShape sh)

-- | Create a delayed array from a function mapping indices to elements.
fromCFunction :: sh -> (CShape sh -> CExp e) -> Array CD sh (CExp e)
fromCFunction = CD

toCFunction :: (Shape sh, IsCArray D sh e)
            => Array D sh (CExp e)
            -> (sh, CShape sh -> CExp e)
toCFunction a =
    case cdelay a of
      CD sh f -> (sh, f)

cdelay :: (Shape sh, IsCArray r sh e)
       => Array r sh (CExp e)
       -> Array CD sh (CExp e)
cdelay a = CD (extent a) (cindex a)

instance Index CD (Z :. Int) (CExp Int) (CExp e) where
    (!) (CD _ f) ci = f (Z :. ci)

instance Index CD (Z :. Int :. Int) (CExp Int, CExp Int) (CExp e) where
    (!) (CD _ f) (ci, cj) = f (Z :. ci :. cj)

instance (Shape sh, ToCShape sh, ToCType e) => IsCArray CD sh e where
    cindex (CD _ f) = f
