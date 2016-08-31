{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Backend.C.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Monad (
    Cg,
    evalCg,

    alwaysUnroll,
    shouldUnroll,

    tell,
    collect,
    collectDefinitions,
    collectDefinitions_,
    collectDecls,
    collectDecls_,
    collectStms,
    collectStms_,
    collectBlock,

    inNewBlock,
    inNewBlock_,

    appendTopDef,
    appendTopDefs,
    appendTopFunDef,
    appendTopFunDefs,
    appendTopDecl,
    appendTopDecls,
    appendDecl,
    appendDecls,
    appendStm,
    appendStms,
    appendBlock,
    appendTopComment,
    appendComment,

    cacheConst,
    cacheCExp,
    lookupCExp,

    cgVar,
    cgFor,

    cgRawTemp,
    CTemp(..)
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Reader (MonadReader(..),
                             ReaderT,
                             asks,
                             runReaderT)
import Control.Monad.State (MonadState(..),
                            StateT,
                            execStateT,
                            gets,
                            modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Complex
import Data.Foldable (toList)
import Data.IORef (IORef)
import Data.Loc (noLoc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

import Spiral.Backend.C.CExp
import Spiral.Backend.C.Code
import Spiral.Backend.C.Types
import Spiral.Backend.C.Util
import Spiral.Config
import Spiral.Globals
import Spiral.Monad (MonadCg)
import Spiral.Trace
import Spiral.Util.Uniq

data CgEnv = CgEnv { unroll :: Bool }

defaultCgEnv :: CgEnv
defaultCgEnv = CgEnv { unroll = False }

data CgState = CgState
    { -- | Generated code
      code :: Code
    , -- | Cached constants
      constCache :: Map C.Initializer C.Exp
    , -- | Cached expressions
      expCache :: Map C.Exp C.Exp
    }

defaultCgState :: CgState
defaultCgState = CgState
    { code       = mempty
    , constCache = mempty
    , expCache   = mempty
    }

-- | The 'Cg' monad transformer.
newtype Cg m a = Cg { unCg :: StateT CgState (ReaderT CgEnv m) a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadReader CgEnv,
              MonadState CgState,
              MonadUnique,
              MonadTrace,
              MonadConfig)

instance MonadTrans Cg where
    lift = Cg . lift . lift

deriving instance MonadRef IORef m => MonadRef IORef (Cg m)

instance PrimMonad m => PrimMonad (Cg m) where
    type PrimState (Cg m) = PrimState m
    primitive = Cg . primitive

instance MonadCg m => MonadCg (Cg m) where

-- | Evaluate a 'Cg' action and return a list of 'C.Definition's.
evalCg :: Monad m => Cg m () -> m (Seq C.Definition)
evalCg m = do
    s <- runReaderT (execStateT (unCg m) defaultCgState) defaultCgEnv
    return $ (codeDefs . code) s <> (codeFunDefs . code) s

-- | Always unroll loop in the continuation.
alwaysUnroll :: Monad m => Cg m a -> Cg m a
alwaysUnroll = local $ \env -> env { unroll = True }

-- | Should we unroll a loop of the given size?
shouldUnroll :: MonadConfig m => Int -> Cg m Bool
shouldUnroll n = do
    always <- asks unroll
    maxun  <- asksConfig maxUnroll
    return $ always || n <= maxun

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

-- | Collect a block of code generated by a computation.
collectBlock :: Monad m => Cg m a -> Cg m (Seq C.InitGroup, Seq C.Stm, a)
collectBlock m = do
    (c, x) <- collect m
    tell c { codeDecls = mempty
           , codeStms  = mempty
           }
    return (codeDecls c, codeStms c, x)

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

cacheConst :: MonadUnique m
           => C.Initializer
           -> C.Type
           -> Cg m C.Exp
cacheConst cinits ctau = do
    maybe_ce <- gets (Map.lookup cinits . constCache)
    case maybe_ce of
      Just ce -> return ce
      Nothing -> do ctemp  <- cgVar "m"
                    let ce =  [cexp|$id:ctemp|]
                    appendTopDecl [cdecl|$ty:ctau $id:ctemp = $init:cinits;|]
                    modify $ \s -> s { constCache = Map.insert cinits ce (constCache s) }
                    return ce

-- | Cache a 'CExp'. This generates a local binding for the value of the 'CExp'.
cacheCExp :: forall a m . (ToCType a, CTemp a (CExp a), MonadCg m)
          => CExp a
          -> Cg m (CExp a)
cacheCExp e0 = do
    maybe_ce' <- gets (Map.lookup ce . expCache)
    case maybe_ce' of
      Just ce' -> return $ CExp ce'
      Nothing  -> go e0
  where
    ce :: C.Exp
    ce = toExp e0 noLoc

    go :: CExp a -> Cg m (CExp a)
    go ce@CInt{} =
        return ce

    go ce@CDouble{} =
        return ce

    go ce@(CComplex CDouble{} CDouble{}) =
        return ce

    go (CComplex cr ci) | not useComplexType = do
        cr' <- cacheCExp cr
        ci' <- cacheCExp ci
        return $ CComplex cr' ci'

    go ce@(CExp [cexp|$id:_|]) =
        return ce

    go ce@(CExp [cexp|-$id:_|]) =
        return ce

    go (CExp [cexp|-$ce1 - $ce2|]) = do
        ce1' <- lookupCExp (CExp [cexp|-$ce1|] :: CExp a)
        ce2' <- lookupCExp (CExp [cexp|-$ce2|] :: CExp a)
        case (ce1', ce2') of
          (CExp [cexp|$id:_|], CExp [cexp|$id:_|]) ->
              return $ CExp [cexp|$ce1' + $ce2'|]
          _ -> do
              ce' <- cacheCExp e'
              return $ CExp [cexp|-$ce'|]
      where
        e' :: CExp a
        e' = CExp [cexp|$ce1 + $ce2|]

    go (CInit ini) = do
        ce <- cacheConst ini (toCType (undefined :: a))
        return $ CExp ce

    go e = do
        ctemp <- cgTemp (undefined :: a)
        appendStm [cstm|$ctemp = $e;|]
        modify $ \s -> s { expCache = Map.insert ce [cexp|$ctemp|] (expCache s) }
        return ctemp

-- | Look up the cached C expression corresponding to a 'CExp'. If the 'CExp'
-- has not been cached, we return it without caching it.
lookupCExp :: forall a m . (ToCType a, MonadUnique m)
           => CExp a
           -> Cg m (CExp a)
lookupCExp e = do
    maybe_ce' <- gets (Map.lookup ce . expCache)
    case maybe_ce' of
      Just ce' -> return $ CExp ce'
      Nothing  -> return e
  where
    ce :: C.Exp
    ce = toExp e noLoc

-- | Generate a unique C identifier name using the given prefix.
cgVar :: MonadUnique m => String -> Cg m C.Id
cgVar = gensym

-- | Generate code for a loop with the given start and end.
cgFor :: MonadCg m
      => Int                   -- ^ Initial value
      -> Int                   -- ^ Upper bound (non-inclusive)
      -> (CExp Int -> Cg m ()) -- ^ Loop body
      -> Cg m ()
cgFor lo hi k = do
    should <- shouldUnroll (hi - lo)
    if should
      then mapM_ k [CInt i | i <- [lo..hi-1::Int]]
      else do
        ci    <- cgVar "i"
        items <- inNewBlock_ $ k (CExp [cexp|$id:ci|])
        appendStm [cstm|for (int $id:ci = $int:lo; $id:ci < $int:hi; ++$id:ci) $stm:(toStm items)|]

-- | Type-directed generation of temporary variables.
class CTemp a b | a -> b where
    -- | Generate a temporary variable.
    cgTemp :: MonadCg m => a -> Cg m b

cgRawTemp :: (ToCType a, MonadCg m) => a -> Cg m C.Exp
cgRawTemp a = do
    t <- cgVar "t"
    appendDecl [cdecl|$ty:ctau $id:t;|]
    return [cexp|$id:t|]
  where
    ctau :: C.Type
    ctau = toCType a

instance CTemp Int (CExp Int) where
    cgTemp x = CExp <$> cgRawTemp x

instance CTemp Double (CExp Double) where
    cgTemp x = CExp <$> cgRawTemp x

instance CTemp (Complex Double) (CExp (Complex Double)) where
    cgTemp x | useComplexType =
        CExp <$> cgRawTemp x

    cgTemp _ =
        CComplex <$> cgTemp (undefined :: Double) <*> cgTemp (undefined :: Double)
