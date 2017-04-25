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
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Program.Monad
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Program.Monad (
    P(..),
    evalP,

    alwaysUnroll,
    shouldUnroll,

    comment,

    (.:=.),
    assignP,
    forP,
    tempP,

    cache,

    newArray,
    cacheArray
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
import Text.PrettyPrint.Mainland

import Data.Heterogeneous
import Spiral.Array
import Spiral.Array.Repr.Hidden
import Spiral.Array.Repr.Complex
import Spiral.Array.Repr.Concrete
import Spiral.Config
import Spiral.Driver.Globals
import Spiral.Exp
import Spiral.Monad (MonadSpiral)
import Spiral.Program.Syntax
import Spiral.Util.Trace
import Spiral.Util.Uniq

data PEnv = PEnv { unroll :: Bool }

defaultPEnv :: PEnv
defaultPEnv = PEnv { unroll = False }

data PState = PState
    { decls  :: Decls
    , stms   :: Stms
    , ecache :: Map (Some Exp) Var
    }

defaultPState :: PState
defaultPState = PState
    { decls  = mempty
    , stms   = mempty
    , ecache = mempty
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

-- | Look up an expression in the expression cache.
lookupCachedExp :: Monad m => Exp a -> P m (Maybe Var)
lookupCachedExp e = Map.lookup (Some e) <$> gets ecache

-- | Insert an expression into the expression cache.
insertCachedExp :: Monad m => Var -> Exp a -> P m ()
insertCachedExp v e =
    modify $ \s -> s { ecache = Map.insert (Some e) v (ecache s) }

-- | Always unroll loops in the given computation.
alwaysUnroll :: Monad m => P m a -> P m a
alwaysUnroll = local $ \env -> env { unroll = True }

-- | Return 'True' if loops of the given size should be unrolled.
shouldUnroll :: MonadSpiral m => Int -> P m Bool
shouldUnroll n = do
    always <- asks unroll
    maxun  <- asksConfig maxUnroll
    return $ always || n <= maxun

-- | Comment on the code being generated.
comment :: MonadSpiral m => Doc -> P m ()
comment doc = whenDynFlag GenComments $ appendStm (CommentS doc)

-- | @'forP' begin end k@ generates code for a fro loop with bounds @begin@
-- and @end@, where the body of the loop is generated by calling theVarDVarD
-- continuation @k@ with an expression representing the loop variable.
forP :: MonadSpiral m => Int -> Int -> (Exp Int -> P m ()) -> P m ()
forP lo hi k = do
    should <- shouldUnroll (hi - lo)
    if should
      then mapM_ k [intE i | i <- [lo..hi-1::Int]]
      else do
        i     <- gensym "i"
        block <- inNewBlock_ $ k (VarE i)
        appendStm $ ForS i lo hi block

-- | Generate a temporary of the given type.
tempP :: forall a m . (Typed a, MonadSpiral m) => P m (Exp a)
tempP = go tau
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

    go :: Type b -> P m (Exp b)
    go (ComplexT tau') | not useComplexType = do
        er <- go tau'
        ei <- go tau'
        return $ ComplexE er ei

    go tau = do
        v <- gensym "t"
        appendDecl $ VarD v tau
        return $ VarE v

-- | An alias for 'assignP'.
infix 4 .:=.
(.:=.) :: (Typed a, Num (Exp a), MonadSpiral m) => Exp a -> Exp a -> P m ()
(.:=.) = assignP

-- | Assign one expression to another.
assignP :: forall a m . (Typed a, Num (Exp a), MonadSpiral m) => Exp a -> Exp a -> P m ()
assignP e1 e2 = do
    appendStm $ AssignS e1 e2
    update e1 e2
  where
    update :: Exp a -> Exp a -> P m ()
    update (VarE v) e = insertCachedExp v e
    update _        _ = return ()

-- | Cache the given expression. This serves as a hint that it will be used
-- more than once.
cache :: (Typed a, Num (Exp a), MonadSpiral m) => Exp a -> P m (Exp a)
cache e@ConstE{} =
    return e

cache e@VarE{} =
    return e

cache (ComplexE er ei) = do
    er' <- cache er
    ei' <- cache ei
    return (ComplexE er' ei')

cache (BinopE Add e1 (UnopE Neg e2)) =
    cache (e1 - e2)

cache (BinopE Add e1 (BinopE Mul c2 e2)) | isNeg c2 =
    cache (e1 - ((-c2) * e2))

cache (BinopE Sub e1 (BinopE Mul c2 e2)) | isNeg c2 =
    cache (e1 + ((-c2) * e2))

cache (BinopE Add (BinopE Mul c1 e1) (BinopE Mul c2 e2))
  | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, epsDiff x1 x2 = do
    e12 <- cache (e1 + e2)
    cache (c1 * e12)

cache (BinopE Add (BinopE Mul c1 e1) (BinopE Mul c2 e2))
  | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, x1 < 0, epsDiff x1 (-x2) = do
    e12 <- cache (e1 - e2)
    cache (c1 * e12)

cache (BinopE Sub (BinopE Mul c1 e1) (BinopE Mul c2 e2))
  | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, x1 < 0, epsDiff x1 (-x2) = do
    e12 <- cache (e1 + e2)
    cache (c1 * e12)

cache (BinopE op e1 e2) = do
    e1' <- cache e1
    e2' <- cache e2
    mustCache (BinopE op e1' e2')

cache (UnopE op e) = do
    e' <- cache e
    mustCache (UnopE op e')

cache e =
    mustCache e

isNeg :: Exp a -> Bool
isNeg (ConstE (DoubleC x)) = x < 0
isNeg _                    = False

epsDiff :: (Ord a, Fractional a) => a -> a -> Bool
epsDiff x y = abs (x - y) < eps
  where
    eps = 1e-15

-- | Really cache the expression.
mustCache :: (Typed a, Num (Exp a), MonadSpiral m ) => Exp a -> P m (Exp a)
mustCache e@VarE{} =
    return e

mustCache e@(UnopE Neg VarE{}) =
    return e

mustCache e = do
    maybe_v <- lookupCachedExp e
    case maybe_v of
      Just v  -> return $ VarE v
      Nothing -> do temp <- tempP
                    assignP temp e
                    return temp

-- | Create a new concrete array of the given shape.
newArray :: forall sh a m . (Shape sh, Typed a, Num (Exp a), MonadSpiral m)
         => (sh :. Int)
         -> P m (Array H (sh :. Int) (Exp a))
newArray (sh :. i) = do
    v <- gensym "V"
    go v (typeOf (undefined :: a))
  where
    go :: Var -> Type a -> P m (Array H (sh :. Int) (Exp a))
    go v (ComplexT tau) | not useComplexType = do
        appendDecl $ ArrD v (sh :. 2*i) tau
        return $ H $ CMPLX $ C (sh :. 2*i) v

    go v tau = do
        appendDecl $ ArrD v (sh :. i) tau
        return $ H $ C (sh :. i) v

-- | Cache the contents of a matrix, returning it as a concrete matrix.
cacheArray :: forall a r sh m . (Typed a, Num (Exp a), Shape sh, IArray r (sh :. Int) (Exp a), MonadSpiral m)
           => Array r (sh :. Int) (Exp a)
           -> P m (Array H (sh :. Int) (Exp a))
cacheArray arr = do
    v <- gensym "K"
    go v (typeOf (undefined :: a))
  where
    go :: Var -> Type a -> P m (Array H (sh :. Int) (Exp a))
    go v ComplexT{} | not useComplexType = do
        appendDecl $ ConstArrD v (RE arr)
        return $ H $ CMPLX $ C (extent (RE arr)) v

    go v _ = do
        appendDecl $ ConstArrD v arr
        return $ H $ C (extent arr) v
