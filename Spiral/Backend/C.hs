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
    let cin :: C.Id  = fromString "in"
    let cout :: C.Id = fromString "out"
    let vin          = fromString "in"
    let vout         = fromString "out"
    extendVars [(vin, CExp [cexp|$id:cin|]), (vout, CExp [cexp|$id:cout|])] $ do
      let x =  P.C (ix1 n) vin
      let y =  P.C (ix1 m) vout
      items <- inNewBlock_ $
               runSPL a (fromGather x) >>= P.computeP y
      appendTopFunDef [cedecl|
void $id:name(const $ty:(restrict (cgArrayType tau (ix1 n))) $id:cin,
             $ty:(restrict (cgArrayType tau (ix1 m))) $id:cout)
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
