{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Program.Monad
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Program.Monad (
    P(..),
    evalP
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
import Data.IORef (IORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence ((|>))

import Data.Heterogeneous
import Spiral.Array
import Spiral.Array.Program
import qualified Spiral.Array.Program as P
import Spiral.Config
import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Program.Syntax
import Spiral.Util.Trace
import Spiral.Util.Uniq

data PEnv = PEnv { unroll :: Bool }

defaultPEnv :: PEnv
defaultPEnv = PEnv { unroll = False }

data PState = PState
    { ecache :: Map (Some Exp) Var
    , decls  :: Decls
    , stms   :: Stms
    }

defaultPState :: PState
defaultPState = PState
    { ecache = mempty
    , decls  = mempty
    , stms   = mempty
    }

newtype P m a = P { unP :: StateT PState (ReaderT PEnv m) a}
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadReader PEnv,
              MonadState PState,
              MonadUnique,
              MonadTrace,
              MonadConfig)

instance MonadTrans P where
    lift = P . lift . lift

deriving instance MonadRef IORef m => MonadRef IORef (P m)

instance PrimMonad m => PrimMonad (P m) where
    type PrimState (P m) = PrimState m
    primitive = P . primitive

instance MonadSpiral m => MonadSpiral (P m) where

evalP :: Monad m => P m () -> m Block
evalP m = do
    s <- runReaderT (execStateT (unP m) defaultPState) defaultPEnv
    return $ Block (decls s) (stms s)

appendDecl :: Monad m => Decl -> P m ()
appendDecl d = modify $ \s -> s { decls = decls s |> d }

appendStm :: Monad m => Stm -> P m ()
appendStm stm = modify $ \s -> s { stms = stms s |> stm }

inNewBlock :: Monad m => P m a -> P m (Block, a)
inNewBlock k = do
    old_decls <- gets decls
    old_stms  <- gets stms
    modify $ \s -> s { decls = mempty, stms = mempty }
    x <- k
    block <- Block <$> gets decls <*> gets stms
    modify $ \s -> s { decls = old_decls, stms = old_stms }
    return (block, x)

inNewBlock_ :: Monad m => P m a -> P m Block
inNewBlock_ m = fst <$> inNewBlock m

instance MonadSpiral m => MonadP (P m) where
    -- | Always unroll loop in the continuation.
    alwaysUnroll = local $ \env -> env { unroll = True }

    -- | Should we unroll a loop of the given size?
    shouldUnroll n = do
        always <- asks unroll
        maxun  <- asksConfig maxUnroll
        return $ always || n <= maxun

    comment doc = whenDynFlag GenComments $ appendStm (CommentS doc)

    forP lo hi k = do
        should <- P.shouldUnroll (hi - lo)
        if should
          then mapM_ k [intE i | i <- [lo..hi-1::Int]]
          else do
            i     <- gensym "i"
            block <- inNewBlock_ $ k (VarE i)
            appendStm $ ForS i lo hi block

    tempP :: forall a . Typed a => P m (Exp a)
    tempP = do
        v <- gensym "t"
        appendDecl $ VarD v tau
        return $ VarE v
      where
        tau :: Type
        tau = typeOf (undefined :: a)

    newArray :: forall sh a . (Shape sh, Typed a)
             => sh
             -> P m (Array C sh (Exp a))
    newArray sh = do
        v <- gensym "V"
        appendDecl $ ArrD v sh tau
        return $ C sh v
      where
        tau :: Type
        tau = typeOf (undefined :: a)

    assignP e1 e2 = do
        appendStm $ AssignS e1 e2
        update e1 e2
      where
        update :: Exp a -> Exp a -> P m ()
        update (VarE v) e = modify $ \s -> s { ecache = Map.insert (Some e) v (ecache s) }
        update _        _ = return ()

    mustCache e@VarE{} =
        return e

    mustCache e@(UnopE Neg VarE{}) =
        return e

    mustCache e = do
        maybe_v <- Map.lookup (Some e) <$> gets ecache
        case maybe_v of
          Just v  -> return $ VarE v
          Nothing -> do temp <- tempP
                        assignP temp e
                        return temp

    cacheArray arr = do
        v <- gensym "K"
        appendDecl $ ConstArrD v arr
        return $ C (extent arr) v
