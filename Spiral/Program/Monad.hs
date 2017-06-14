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

    cacheExp,

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
tempP = do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    go useComplexType tau
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

    go :: Bool -> Type b -> P m (Exp b)
    go False (ComplexT tau') = do
        er <- go False tau'
        ei <- go False tau'
        return $ ComplexE er ei

    go _ tau = do
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
    e2' <- cacheExp e2
    assignS e1 e2'

assignS :: (Typed a, MonadSpiral m) => Exp a -> Exp a -> P m ()
assignS e1@(VarE v) e2 = do
    insertCachedExp v e2
    appendStm $ AssignS e1 e2

assignS (ComplexE e1r e1i) (ComplexE e2r e2i) = do
    assignS e1r e2r
    assignS e1i e2i

assignS e1 e2 =
    appendStm $ AssignS e1 e2

-- | Cache the given expression. This serves as a hint that it will be used
-- more than once.
cacheExp :: forall a m . (Typed a, Num (Exp a), MonadSpiral m) => Exp a -> P m (Exp a)
cacheExp e = do
    config <- askConfig
    cacheWithConfig config e

cacheWithConfig :: forall a m . (Typed a, Num (Exp a), MonadSpiral m)
                => Config
                -> Exp a
                -> P m (Exp a)
cacheWithConfig config = cache
  where
    useComplex, splitComplex, doCSE, doRewrite :: Bool
    useComplex   = UseComplex `testDynFlag` config
    splitComplex = SplitComplex `testDynFlag` config
    doCSE        = CSE `testDynFlag` config
    doRewrite    = Rewrite `testDynFlag` config

    cache :: forall a . (Typed a, Num (Exp a)) => Exp a -> P m (Exp a)
    cache e
      | doRewrite = rewrite e
      | otherwise = mustCache e

    rewrite :: forall a . (Typed a, Num (Exp a)) => Exp a -> P m (Exp a)
    rewrite (ConstE (FloatC x)) | x < 0 =
        return $ UnopE Neg (ConstE (FloatC (-x)))

    rewrite (ConstE (DoubleC x)) | x < 0 =
        return $ UnopE Neg (ConstE (DoubleC (-x)))

    rewrite e@ConstE{} =
        return e

    rewrite e@VarE{} =
        return e

    rewrite (ComplexE er ei) =
        ComplexE <$> rewrite er <*> rewrite ei

    -- Don't cache negated constants. We don't want to fall through to the next
    -- case because the call to 'negate' will re-negate the constant!
    rewrite e@(UnopE Neg ConstE{}) =
        return e

    -- Cache the term we are negating, not the negated term.
    rewrite (UnopE Neg e) =
        negate <$> rewrite e

    rewrite (UnopE op e) = do
        e' <- rewrite e
        mustCache (UnopE op e')

    rewrite (BinopE op e1 e2) = do
        e1' <- rewrite e1
        e2' <- rewrite e2
        -- Re-simplify expressions after caching their subterms if the subterms
        -- have changed.
        if e1' /= e1 || e2' /= e2
          then case op of
                 Add -> rewrite (e1' + e2')
                 Sub -> rewrite (e1' - e2')
                 Mul -> rewrite (e1' * e2')
                 _   -> go op e1' e2'
          else go op e1' e2'
      where
        go :: Binop -> Exp a -> Exp a -> P m (Exp a)
        -- Choose canonical variable ordering
        go Add e1@VarE{} e2@VarE{} | e2 < e1 =
            mustCache (BinopE Add e2 e1)

        go Add e1@IdxE{} e2@IdxE{} | e2 < e1 =
            mustCache (BinopE Add e2 e1)

        go Sub e1@VarE{} e2@VarE{} | e2 < e1 =
            UnopE Neg <$> mustCache (BinopE Sub e2 e1)

        go Sub e1@IdxE{} e2@IdxE{} | e2 < e1 =
            UnopE Neg <$> mustCache (BinopE Sub e2 e1)

        go Mul e1@VarE{} e2@VarE{} | e2 < e1 =
            mustCache (BinopE Mul e2 e1)

        go Mul e1@IdxE{} e2@IdxE{} | e2 < e1 =
            mustCache (BinopE Mul e2 e1)

        -- Constants always come first
        go Mul e1 e2@ConstE{} | not (isConstE e1) =
            go Mul e2 e1

        -- Push negation out
        go Add e1 (UnopE Neg e2) =
            rewrite $ e1 - e2

        go Add (UnopE Neg e1) e2 =
            rewrite $ e2 - e1

        go Sub e1 (UnopE Neg e2) =
            rewrite $ e1 + e2

        go Sub (UnopE Neg e1) e2 =
            rewrite $ -(e1 + e2)

        go Mul e1 (UnopE Neg e2) =
            rewrite $ -(e1 * e2)

        go Mul (UnopE Neg e1) e2 =
            rewrite $ -(e1 * e2)

        go op e1 e2 =
            mustCache (BinopE op e1 e2)

    rewrite (ReE e) =
        ReE <$> rewrite e >>= mustCache

    rewrite (ImE e) =
        ImE <$> rewrite e >>= mustCache

    rewrite e =
        mustCache e

    -- When we are not using complex types, always separately cache the real and
    -- imaginary parts of a complex expression.
    mustCache :: forall a . (Typed a, Num (Exp a)) => Exp a -> P m (Exp a)
    mustCache (ComplexE er ei) =
        ComplexE <$> mustCache er <*> mustCache ei

    mustCache (ReE e) | not useComplex, splitComplex = do
        let (er, _ei) = forceUnComplexE e
        cache er

    mustCache (ImE e) | not useComplex, splitComplex = do
        let (_er, ei) = forceUnComplexE e
        cache ei

    mustCache e | not useComplex, splitComplex, ComplexT{} <- tau = do
        let (er, ei) = forceUnComplexE e
        ComplexE <$> cache er <*> cache ei >>= cache
      where
        tau :: Type a
        tau = typeOf (undefined :: a)

    mustCache e | doCSE = do
        maybe_v <- lookupCachedExp e
        case maybe_v of
          Just v  -> return $ VarE v
          Nothing -> do temp <- tempP
                        assignS temp e
                        return temp

    mustCache e =
        return e

-- | Create a new concrete array of the given shape.
newArray :: forall sh a m . (Shape sh, Typed a, Num (Exp a), MonadSpiral m)
         => (sh :. Int)
         -> P m (Array H (sh :. Int) (Exp a))
newArray (sh :. i) = do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    v <- gensym "V"
    go useComplexType v (typeOf (undefined :: a))
  where
    go :: Bool -> Var -> Type a -> P m (Array H (sh :. Int) (Exp a))
    go False v (ComplexT tau) = do
        appendDecl $ ArrD v (sh :. 2*i) tau
        return $ H $ CMPLX $ C (sh :. 2*i) v

    go _ v tau = do
        appendDecl $ ArrD v (sh :. i) tau
        return $ H $ C (sh :. i) v

-- | Cache the contents of a matrix, returning it as a concrete matrix.
cacheArray :: forall a r sh m . (Typed a, Num (Exp a), Shape sh, IArray r (sh :. Int) (Exp a), MonadSpiral m)
           => Array r (sh :. Int) (Exp a)
           -> P m (Array H (sh :. Int) (Exp a))
cacheArray arr = do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    v <- gensym "K"
    go useComplexType v (typeOf (undefined :: a))
  where
    go :: Bool -> Var -> Type a -> P m (Array H (sh :. Int) (Exp a))
    go False v ComplexT{} = do
        appendDecl $ ConstArrD v (RE arr)
        return $ H $ CMPLX $ C (extent (RE arr)) v

    go _ v _ = do
        appendDecl $ ConstArrD v arr
        return $ H $ C (extent arr) v
