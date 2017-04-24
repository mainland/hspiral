{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Backend.C.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Monad (
    Cg,
    evalCg,

    tell,
    collect,
    collectDefinitions,
    collectDefinitions_,
    collectDecls,
    collectDecls_,
    collectStms,
    collectStms_,
    collectFunBlock,
    collectBlock,

    inNewFunBlock,
    inNewFunBlock_,
    inNewBlock,
    inNewBlock_,

    appendTopDef,
    appendTopDefs,
    appendTopFunDef,
    appendTopFunDefs,
    appendTopDecl,
    appendTopDecls,
    appendFunDecl,
    appendFunDecls,
    appendDecl,
    appendDecls,
    appendStm,
    appendStms,
    appendBlock,
    appendTopComment,
    appendComment,

    cacheConst,

    extendVars,
    insertVar,
    lookupVar,

    cgId,
    cgType,
    cgArrayType,
    cgAssign
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (MonadState(..),
                            StateT,
                            execStateT,
                            gets,
                            modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.List (foldl')
import Data.Loc (noLoc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.C.Pretty ()
import qualified Language.C.Quote as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Code
import Spiral.Config
import Spiral.Driver.Globals
import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Util.Trace
import Spiral.Util.Uniq

data CgState = CgState
    { -- | Generated code
      code :: Code
    , -- | Compiled variables
      vars :: Map Var CExp
    , -- | Cached compiler constants
      cinitCache :: Map C.Initializer C.Exp
    }

defaultCgState :: CgState
defaultCgState = CgState
    { code       = mempty
    , vars       = mempty
    , cinitCache = mempty
    }

-- | The 'Cg' monad transformer.
newtype Cg m a = Cg { unCg :: StateT CgState m a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadState CgState,
              MonadUnique,
              MonadTrace,
              MonadConfig)

instance MonadTrans Cg where
    lift = Cg . lift

deriving instance MonadRef IORef m => MonadRef IORef (Cg m)

instance PrimMonad m => PrimMonad (Cg m) where
    type PrimState (Cg m) = PrimState m
    primitive = Cg . primitive

instance MonadSpiral m => MonadSpiral (Cg m) where

-- | Evaluate a 'Cg' action and return a list of 'C.Definition's.
evalCg :: Monad m => Cg m () -> m (Seq C.Definition)
evalCg m = do
    s <- execStateT (unCg m) defaultCgState
    return $ (codeDefs . code) s <> (codeFunDefs . code) s

-- | Add generated code.
tell :: Monad m => Code -> Cg m ()
tell c = modify $ \s -> s { code = code s <> c }

-- | Collect code generated by a computation.
collect :: Monad m => Cg m a -> Cg m (Code, a)
collect m = do
    old_code <- gets code
    modify $ \s -> s { code = mempty }
    x <- m
    c <- gets code
    modify $ \s -> s { code = old_code }
    return (c, x)

-- | Collect definitions generated by a computation.
collectDefinitions :: Monad m => Cg m a -> Cg m ([C.Definition], a)
collectDefinitions m = do
    (c, x) <- collect m
    tell c { codeDefs = mempty }
    return (toList (codeDefs c), x)

-- | Collect definitions generated by a unit computation.
collectDefinitions_ :: Monad m => Cg m () -> Cg m [C.Definition]
collectDefinitions_ m = fst <$> collectDefinitions m

-- | Collect declarations generated by a computation.
collectDecls :: Monad m => Cg m a -> Cg m ([C.InitGroup], a)
collectDecls m = do
    (c, x) <- collect m
    tell c { codeDecls = mempty }
    return (toList (codeDecls c), x)

-- | Collect declarations generated by a unit computation.
collectDecls_ :: Monad m => Cg m () -> Cg m [C.InitGroup]
collectDecls_ m = fst <$> collectDecls m

-- | Collect statements generated by a computation.
collectStms :: Monad m => Cg m a -> Cg m ([C.Stm], a)
collectStms m = do
    (c, x) <- collect m
    tell c { codeStms = mempty }
    return (toList (codeStms c), x)

-- | Collect statements generated by a unit computation.
collectStms_ :: Monad m => Cg m () -> Cg m [C.Stm]
collectStms_ m = fst <$> collectStms m

-- | Collect the body of a function.
collectFunBlock :: Monad m => Cg m a -> Cg m (Seq C.InitGroup, Seq C.Stm, a)
collectFunBlock m = do
    (c, x) <- collect m
    tell c { codeFunDecls = mempty
           , codeDecls    = mempty
           , codeStms     = mempty
           }
    return (codeFunDecls c <> codeDecls c, codeStms c, x)

-- | Collect a block of code generated by a computation.
collectBlock :: Monad m => Cg m a -> Cg m (Seq C.InitGroup, Seq C.Stm, a)
collectBlock m = do
    (c, x) <- collect m
    tell c { codeDecls = mempty
           , codeStms  = mempty
           }
    return (codeDecls c, codeStms c, x)

inNewFunBlock :: Monad m => Cg m a -> Cg m ([C.BlockItem], a)
inNewFunBlock m = do
    (decls, stms, x) <- collectFunBlock m
    return ((map C.BlockDecl . toList) decls ++
            (map C.BlockStm .  toList) stms
           ,x)

inNewFunBlock_ :: Monad m => Cg m a -> Cg m [C.BlockItem]
inNewFunBlock_ m =
    fst <$> inNewFunBlock m

inNewBlock :: Monad m => Cg m a -> Cg m ([C.BlockItem], a)
inNewBlock m = do
    (decls, stms, x) <- collectBlock m
    return ((map C.BlockDecl . toList) decls ++
            (map C.BlockStm .  toList) stms
           ,x)

inNewBlock_ :: Monad m => Cg m a -> Cg m [C.BlockItem]
inNewBlock_ m =
    fst <$> inNewBlock m

-- | Append a top-level definition.
appendTopDef :: Monad m => C.Definition -> Cg m ()
appendTopDef cdef =
  tell mempty { codeDefs = Seq.singleton cdef }

-- | Append top-level definitions.
appendTopDefs :: Monad m => [C.Definition] -> Cg m ()
appendTopDefs cdefs =
  tell mempty { codeDefs = Seq.fromList cdefs }

-- | Append a top-level function definition. Function definitions appear after
-- regular top-level definitions.
appendTopFunDef :: Monad m => C.Definition -> Cg m ()
appendTopFunDef cdef =
  tell mempty { codeFunDefs = Seq.singleton cdef }

-- | Append top-level functions definitions.
appendTopFunDefs :: Monad m => [C.Definition] -> Cg m ()
appendTopFunDefs cdefs =
  tell mempty { codeFunDefs = Seq.fromList cdefs }

appendTopDecl :: Monad m => C.InitGroup -> Cg m ()
appendTopDecl cdecl =
  tell mempty { codeDefs = Seq.singleton (C.DecDef cdecl noLoc) }

appendTopDecls :: Monad m => [C.InitGroup] -> Cg m ()
appendTopDecls cdecls =
  tell mempty { codeDefs = Seq.fromList [C.DecDef decl noLoc | decl <- cdecls] }

appendFunDecl :: Monad m => C.InitGroup -> Cg m ()
appendFunDecl cdecl = tell mempty { codeFunDecls = Seq.singleton cdecl }

appendFunDecls :: Monad m => [C.InitGroup] -> Cg m ()
appendFunDecls cdecls = tell mempty { codeFunDecls = Seq.fromList cdecls }

appendDecl :: Monad m => C.InitGroup -> Cg m ()
appendDecl cdecl = tell mempty { codeDecls = Seq.singleton cdecl }

appendDecls :: Monad m => [C.InitGroup] -> Cg m ()
appendDecls cdecls = tell mempty { codeDecls = Seq.fromList cdecls }

appendStm :: Monad m => C.Stm -> Cg m ()
appendStm cstm = tell mempty { codeStms = Seq.singleton cstm }

appendStms :: Monad m => [C.Stm] -> Cg m ()
appendStms cstms = tell mempty { codeStms = Seq.fromList cstms }

appendBlock :: Monad m => [C.BlockItem] -> Cg m ()
appendBlock citems
    | all isBlockStm citems = appendStms [stm | C.BlockStm stm <- citems]
    | otherwise             = appendStm [cstm|{ $items:citems }|]
  where
    isBlockStm :: C.BlockItem -> Bool
    isBlockStm C.BlockStm{} = True
    isBlockStm _            = False

-- | Append a comment to the list of top-level definitions.
appendTopComment :: Monad m => Doc -> Cg m ()
appendTopComment doc = appendTopDef [cedecl|$esc:(formatComment doc)|]

-- | Append a comment to the current sequence of statements.
appendComment :: Monad m => Doc -> Cg m ()
appendComment doc = appendStm [cstm|$escstm:(formatComment doc)|]

formatComment :: Doc -> String
formatComment doc =
    pretty 80 $ group $
    text "/*" <+> align doc </> text "*/"

extendVars :: MonadSpiral m
           => [(Var, CExp)]
           -> Cg m a
           -> Cg m a
extendVars vces m = do
    old_vars <- gets vars
    modify $ \s -> s { vars = foldl' insert (vars s) vces }
    x <- m
    modify $ \s -> s { vars = old_vars }
    return x
  where
    insert :: Ord k => Map k v -> (k, v) -> Map k v
    insert mp (k, v) = Map.insert k v mp

insertVar :: MonadSpiral m
          => Var
          -> CExp
          -> Cg m ()
insertVar v ce =
    modify $ \s -> s { vars = Map.insert v ce (vars s) }

lookupVar :: MonadSpiral m => Var -> Cg m CExp
lookupVar v = do
    maybe_ce <- gets (Map.lookup v . vars)
    case maybe_ce of
      Nothing -> faildoc $ text "Unbound variable:" <+> ppr v
      Just ce -> return ce

-- | Cache the value of a constant.
cacheConst :: MonadUnique m
           => C.Initializer
           -> C.Type
           -> Cg m C.Exp
cacheConst cinits ctau = do
    maybe_ce <- gets (Map.lookup cinits . cinitCache)
    case maybe_ce of
      Just ce -> return ce
      Nothing -> do ctemp  <- cgId "K"
                    let ce =  [cexp|$id:ctemp|]
                    appendTopDecl [cdecl|$ty:ctau $id:ctemp = $init:cinits;|]
                    modify $ \s -> s { cinitCache = Map.insert cinits ce (cinitCache s) }
                    return ce

-- | Generate a unique C identifier name using the given prefix.
cgId :: MonadUnique m => String -> Cg m C.Id
cgId = gensym

-- | Compile a type.
cgType :: Type -> C.Type
cgType IntT     = [cty|int|]
cgType IntegerT = [cty|int|]
cgType DoubleT  = [cty|double|]

cgType (ComplexT DoubleT)
    | useComplexType = [cty|double _Complex|]
    | otherwise      = error "Cannot convert complex double to C type"

cgType tau@ComplexT{} = errordoc $ text "Illegal type:" <+> (text . show) tau

-- | Compile an array type.
cgArrayType :: forall sh . Shape sh => Type -> sh -> C.Type
cgArrayType (ComplexT DoubleT) sh | not useComplexType =
    case listOfShape sh of
      n:sh0 -> cgArrayType DoubleT (shapeOfList (2*n:sh0) :: sh)
      _     -> error "zero-dimensional array of Complex Double"

cgArrayType tau sh = foldl cidx (cgType tau) (listOfShape sh)
  where
    cidx :: C.Type -> Int -> C.Type
    cidx ctau i = [cty|$ty:ctau[$int:i]|]

-- | Compile an assignment.
cgAssign :: MonadSpiral m => Type -> CExp -> CExp -> Cg m ()
cgAssign (ComplexT DoubleT) ce1 ce2 | not useComplexType = do
    appendStm [cstm|$cr1 = $cr2;|]
    appendStm [cstm|$ci1 = $ci2;|]
  where
    (cr1, ci1) = unComplex ce1
    (cr2, ci2) = unComplex ce2

cgAssign _ ce1 ce2 =
    appendStm [cstm|$ce1 = $ce2;|]
