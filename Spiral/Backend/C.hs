{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
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
    cgProgram
  ) where

import Prelude hiding ((!!))

import Data.Foldable (toList)
import Data.Modular
import Data.Symbol
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array hiding (concat,
                            toList)
import Spiral.Array.Repr.Concrete
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Util
import Spiral.Config
import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Program.Syntax
import Spiral.Util.Name
import Spiral.Util.Trace
import Spiral.Util.Uniq

cgProgram :: forall a m . (Num (Exp a), Typed a, MonadSpiral m)
          => Program a
          -> Cg m ()
cgProgram t@(Program f (C n x) (C m y) block) = do
    traceCg $ text "Compiling:" </> ppr t
    appendTopDef [cedecl|$esc:("#include <complex.h>")|]
    appendTopDef [cedecl|$esc:("#include <stdint.h>")|]
    let cx = C.Id "X"
        cy = C.Id "Y"
    items <- inNewFunBlock_ $
             extendVars [(x, CExp [cexp|$id:cx|]), (y, CExp [cexp|$id:cy|])] $
             cgBlock block
    appendTopFunDef [cedecl|
void $id:f(const $ty:(mkParamType (cgArrayType tau n)) $id:cx,
           $ty:(mkParamType (cgArrayType tau m)) $id:cy)
{
  $items:items
}|]
   where
     tau :: Type a
     tau = typeOf (undefined :: a)

     mkParamType :: C.Type -> C.Type
     mkParamType (C.Type dspec decl s) = C.Type dspec (mkRestrict decl) s
       where
         mkRestrict :: C.Decl -> C.Decl
         mkRestrict (C.Ptr quals d s) =
             C.Ptr (C.Trestrict s:quals) (mkRestrict d) s

         mkRestrict (C.Array quals sz d s) =
             C.Array (C.Trestrict s:quals) (mkStatic sz) (mkRestrict d) s

         mkRestrict d =
             d

         mkStatic :: C.ArraySize -> C.ArraySize
         mkStatic (C.ArraySize _ e l) = C.ArraySize True e l
         mkStatic sz                  = sz

     mkParamType ty = ty

cgBlock :: forall m . MonadSpiral m => Block -> Cg m ()
cgBlock (Block decls stms) = cgDecls decls $ cgStms stms

cgDecls :: forall m a . MonadSpiral m => Decls -> Cg m a -> Cg m a
cgDecls decls k = foldr cgDecl k (toList decls)

cgDecl :: MonadSpiral m => Decl -> Cg m a -> Cg m a
cgDecl (VarD v tau) k = do
    cv <- cgVar v
    appendDecl [cdecl|$ty:(cgType tau) $id:cv;|]
    extendVars [(v, CExp [cexp|$id:cv|])] k

cgDecl (ArrD v sh tau) k = do
    cv <- cgVar v
    appendDecl [cdecl|$ty:(cgArrayType tau sh) $id:cv;|]
    extendVars [(v, CExp [cexp|$id:cv|])] k

cgDecl (ConstArrD v (arr :: Array r sh (Exp a))) k = do
    cinit <- cgArrayInit (manifest arr)
    carr  <- CExp <$> cacheConst cinit [cty|static const $ty:ctau |]
    extendVars [(v, carr)] k
  where
      sh = extent arr

      ctau :: C.Type
      ctau = cgArrayType (typeOf (undefined :: a)) sh

cgStms :: forall m . MonadSpiral m => Stms -> Cg m ()
cgStms = mapM_ cgStm

cgStm :: forall m . MonadSpiral m => Stm -> Cg m ()
cgStm (AssignS (e1 :: Exp a) (e2 :: Exp a)) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    cgAssign tau ce1 ce2
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

cgStm (CommentS doc) =
    whenDynFlag GenComments $
    appendComment doc

cgStm (ForS v lo hi block) = do
    cv    <- cgVar v
    items <- inNewBlock_ $
             extendVars [(v, CExp [cexp|$id:cv|])] $
             cgBlock block
    appendStm [cstm|for (int $id:cv = $int:lo; $id:cv < $int:hi; ++$id:cv) $stm:(toStm items)|]

cgArrayInit :: forall a sh m . (Shape sh, Typed a, MonadSpiral m)
            => Array M sh (Exp a)
            -> Cg m C.Initializer
cgArrayInit a = do
    inits <- f (reverse (listOfShape (extent a))) []
    case inits of
      [init] -> return init
      _      -> return [cinit|{ $inits:inits }|]
  where
    f :: [Int] -> [Int] -> Cg m [C.Initializer]
    f [] ix = do
        ce <- cgExp (index a (shapeOfList ix))
        return [toInitializer ce]

    f (n:ix) ix' = do
        inits <- mapM (\i -> f ix (i : ix')) [0..n-1]
        return [[cinit|{ $inits:(concat inits) }|]]

-- | Compiled a value of type 'Const a'.
cgConst :: Const a -> CExp
cgConst (BoolC x)        = CInt $ if x then 1 else 0
cgConst (IntC x)         = CInt x
cgConst (IntegerC x)     = CInt (fromIntegral x)
cgConst (RationalC x)    = CDouble (fromRational x)
cgConst (FloatC x)       = CFloat x
cgConst (DoubleC x)      = CDouble x
cgConst (ModularC x)     = CLLInt (unMod x)
cgConst (ComplexC e1 e2) = CComplex (cgConst e1) (cgConst e2)
cgConst e@W{}            = cgConst (lower e)
cgConst e@CycC{}         = cgConst (lower e)
cgConst e@PiC{}          = cgConst (lower e)

-- | Compile an 'Exp a'.
cgExp :: forall a m . (Typed a, MonadSpiral m) => Exp a -> Cg m CExp
cgExp (ConstE c) = return $ cgConst c
cgExp (VarE v)   = lookupVar v

cgExp (UnopE op e) = do
    ce <- cgExp e
    go op (typeOf (undefined :: a)) ce
  where
    go :: Unop -> Type a -> CExp -> Cg m CExp
    go Neg (ModPT p) ce    = return $ -ce `rem` fromIntegral p
    go Abs (ModPT p) ce    = return $ abs ce `rem` fromIntegral p
    go Signum (ModPT p) ce = return $ signum ce `rem` fromIntegral p

    go op ModPT{} _ =
        faildoc $ text "Cannot compile" <+> ppr op <+> text "for modular arithmetic"

    go Neg _ ce    = return $ -ce
    go Abs _ ce    = return $ abs ce
    go Signum _ ce = return $ signum ce
    go Exp _ ce    = return $ exp ce
    go Log _ ce    = return $ log ce
    go Sqrt _ ce   = return $ sqrt ce
    go Sin _ ce    = return $ sin ce
    go Cos _ ce    = return $ cos ce
    go Asin _ ce   = return $ asin ce
    go Acos _ ce   = return $ acos ce
    go Atan _ ce   = return $ atan ce
    go Sinh _ ce   = return $ sinh ce
    go Cosh _ ce   = return $ cosh ce
    go Asinh _ ce  = return $ asinh ce
    go Acosh _ ce  = return $ acosh ce
    go Atanh _ ce  = return $ atanh ce

cgExp (BinopE op e1 e2) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    go op (typeOf (undefined :: a)) ce1 ce2
  where
    go :: Binop -> Type a -> CExp -> CExp -> Cg m CExp
    go Add (ModPT p) ce1 ce2 = return $ (ce1 + ce2) `rem` fromIntegral p
    go Sub (ModPT p) ce1 ce2 = return $ (ce1 - ce2) `rem` fromIntegral p
    go Mul (ModPT p) ce1 ce2 = return $ (ce1 * ce2) `rem` fromIntegral p

    go op ModPT{} _ _ =
        faildoc $ text "Cannot compile" <+> ppr op <+> text "for modular arithmetic"

    go Add  _ ce1 ce2 = return $ ce1 + ce2
    go Sub  _ ce1 ce2 = return $ ce1 - ce2
    go Mul  _ ce1 ce2 = return $ ce1 * ce2
    go Quot _ ce1 ce2 = return $ ce1 `quot` ce2
    go Rem  _ ce1 ce2 = return $ ce1 `rem` ce2
    go Div  _ ce1 ce2 = do warning $ text "Saw div; consider quot"
                           return $ ce1 `div` ce2
    go Mod  _ ce1 ce2 = do warning $ text "Saw mod; consider rem"
                           return $ ce1 `mod` ce2
    go FDiv _ _   _   = fail "Can't happen"

cgExp (IdxE v es) = do
    cv  <- lookupVar v
    ces <- mapM cgExp es
    return $ CExp $ foldr cidx [cexp|$cv|] ces
  where
    cidx ci ce = [cexp|$ce[$ci]|]

cgExp (ComplexE er ei) =
    CComplex <$> cgExp er <*> cgExp ei

cgExp (ReE e) = do
    (cr, _ci) <- unComplex <$> cgExp e
    return cr

cgExp (ImE e) = do
    (_cr, ci) <- unComplex <$> cgExp e
    return ci

cgExp (BBinopE op e1 e2) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    go op ce1 ce2
  where
    go Eq ce1 ce2 = return $ ce1 .==. ce2
    go Ne ce1 ce2 = return $ ce1 ./=. ce2
    go Lt ce1 ce2 = return $ ce1 .<.  ce2
    go Le ce1 ce2 = return $ ce1 .<=. ce2
    go Ge ce1 ce2 = return $ ce1 .>=. ce2
    go Gt ce1 ce2 = return $ ce1 .>.  ce2

cgExp (IfE e1 e2 e3) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    ce3 <- cgExp e3
    return $ if ce1 then ce2 else ce3

-- | Generate a unique C identifier name for the given variable.
cgVar :: MonadUnique m => Var -> Cg m C.Id
cgVar v = cgId (unintern (namedSymbol v))
