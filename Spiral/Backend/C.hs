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

import Data.String
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Spiral.Array
import qualified Spiral.Array.Program as P
import Spiral.Array.Repr.Transform
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Exp
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Trace

-- | Generate code for an SPL transform.
codegen :: forall a m . (Num (Exp a), Typed a, MonadCg m)
        => String
        -> SPL (Exp a)
        -> Cg m ()
codegen name a = do
    traceCg $ text "Compiling:" </> ppr a
    appendTopDef [cedecl|$esc:("#include <complex.h>")|]
    let cx :: C.Id = fromString "X"
    let cy :: C.Id = fromString "Y"
    let vx         = fromString "X"
    let vy         = fromString "Y"
    extendVars [(vx, CExp [cexp|$id:cx|]), (vy, CExp [cexp|$id:cy|])] $ do
      let x =  P.C (ix1 n) vx
      let y =  P.C (ix1 m) vy
      items <- inNewBlock_ $
               runSPL a (fromGather x) >>= P.computeP y
      appendTopFunDef [cedecl|
void $id:name(const $ty:(restrict (cgArrayType tau (ix1 n))) $id:cx,
                    $ty:(restrict (cgArrayType tau (ix1 m))) $id:cy)
{
  $items:items
}|]
   where
     Z :. m :. n = splExtent a

     tau :: Type
     tau = typeOf (undefined :: a)

     restrict :: C.Type -> C.Type
     restrict (C.Type dspec decl s) =
         C.Type dspec (restrictDecl decl) s
       where
         restrictDecl :: C.Decl -> C.Decl
         restrictDecl (C.Ptr quals d s) =
             C.Ptr (C.Trestrict s:quals) (restrictDecl d) s

         restrictDecl (C.Array quals sz d s) =
             C.Array (C.Trestrict s:quals) sz (restrictDecl d) s

         restrictDecl d =
             d

     restrict ty = ty
