{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.OpCount
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.OpCount (
    OpCount(..),
    countOps,

    Op(..),
    evalOpCount,
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState(..),
                            StateT,
                            execStateT,
                            gets,
                            modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.Mainland

import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Program
import Spiral.SPL
import Spiral.SPL.Run

data OpCount a = OpCount
    { unops  :: Map Unop a
    , binops :: Map Binop a
    }
  deriving (Eq, Ord, Show)

instance Functor OpCount where
    fmap f o = o { unops  = fmap f (unops o)
                 , binops = fmap f (binops o)
                 }

instance Num a => Monoid (OpCount a) where
    mempty = OpCount mempty mempty

    x `mappend` y = OpCount
        { unops  = Map.unionWith (+) (unops x) (unops y)
        , binops = Map.unionWith (+) (binops x) (binops y)
        }

countOps :: (Typed a, Num (Exp a), MonadSpiral m)
         => SPL (Exp a)
         -> m (OpCount Int)
countOps e = do
    prog <- toProgram "f" e
    evalOpCount $ countProgram prog

countProgram :: MonadSpiral m => Program a -> Op m ()
countProgram (Program _ _ _ block) = countBlock block

countStm :: MonadSpiral m => Stm -> Op m ()
countStm (AssignS _e1 e2) =
    countExp e2

countStm CommentS{} =
    return ()

countStm (ForS _ lo hi body) = do
    count <- collect_ $ countBlock body
    tell $ fmap ((hi - lo) *) count

countBlock :: MonadSpiral m => Block -> Op m ()
countBlock (Block _ stms) = mapM_ countStm stms

countExp :: MonadSpiral m => Exp a -> Op m ()
countExp ConstE{} = return ()
countExp VarE{}   = return ()

countExp (UnopE op e) = do
    countExp e
    unop op

countExp (BinopE op e1 e2) = do
    countExp e1
    countExp e2
    binop op

countExp (IdxE _ es) =
    mapM_ countExp es

countExp (ComplexE er ei) = do
    countExp er
    countExp ei

countExp (ReE e) = countExp e
countExp (ImE e) = countExp e

newtype OpState = OpState { counts :: OpCount Int }

defaultOpState :: OpState
defaultOpState = OpState { counts = mempty }

newtype Op m a = Op { unOp :: StateT OpState m a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadState OpState)

instance MonadTrans Op where
    lift = Op . lift

evalOpCount :: Monad m => Op m () -> m (OpCount Int)
evalOpCount m =
    counts <$> execStateT (unOp m) defaultOpState

modifyOpCount :: Monad m => (OpCount Int -> OpCount Int) -> Op m ()
modifyOpCount f =
    modify $ \s -> s { counts = f (counts s) }

unop :: Monad m => Unop -> Op m ()
unop op = modifyOpCount $ \s -> s { unops = Map.alter f op (unops s) }
  where
    f :: Maybe Int -> Maybe Int
    f Nothing  = return 1
    f (Just x) = return $ x + 1

binop :: Monad m => Binop -> Op m ()
binop op = modifyOpCount $ \s -> s { binops = Map.alter f op (binops s) }
  where
    f :: Maybe Int -> Maybe Int
    f Nothing  = return 1
    f (Just x) = return $ x + 1

tell :: Monad m => OpCount Int -> Op m ()
tell c2 = modifyOpCount $ \c1 -> c1 <> c2

collect :: Monad m => Op m a -> Op m (OpCount Int, a)
collect k = do
    old_counts <- gets counts
    modify $ \s -> s { counts = mempty }
    x <- k
    c <- gets counts
    modify $ \s -> s { counts = old_counts }
    return (c, x)

collect_ :: Monad m => Op m () -> Op m (OpCount Int)
collect_ k = fst <$> collect k
