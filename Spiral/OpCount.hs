{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.OpCount
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.OpCount (
    OpCount(..),
    countOps,

    Op(..),
    evalOpCount,
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
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Array.Program
import Spiral.Array.Repr.Transform
import Spiral.Config
import Spiral.Driver.Globals
import Spiral.Driver.Monad (MonadSpiral)
import Spiral.Exp
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Pretty
import Spiral.Util.Trace
import Spiral.Util.Uniq

-- Note that we end up duplicating a lot of the functionality of the C backend
-- in our attempt to accurately count operations. In particular, when we find an
-- expression in the cache, we must do a little abstract interpretation to fold
-- negations. This happens in our 'Num' instance for 'CExp', just as it happens
-- in the 'Num' instance of the C backend's CExp data type. It would be nice to
-- eliminate this duplication!

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

data OpState = OpState
    { -- | Operation counts
      counts    :: OpCount Int
    , -- | Compiled variables
      vars :: Map Var CExp
    , -- | Cached compiled expressions
      cexpCache :: Map CExp CExp
    }

defaultOpState :: OpState
defaultOpState = OpState
    { counts    = mempty
    , vars      = mempty
    , cexpCache = mempty
    }

data OpEnv = OpEnv { unroll :: Bool }

defaultOpEnv :: OpEnv
defaultOpEnv = OpEnv { unroll = False }

newtype Op m a = Op { unOp :: StateT OpState (ReaderT OpEnv m) a }
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadException,
              MonadReader OpEnv,
              MonadState OpState,
              MonadUnique,
              MonadTrace,
              MonadConfig)

instance MonadTrans Op where
    lift = Op . lift . lift

deriving instance MonadRef IORef m => MonadRef IORef (Op m)

instance PrimMonad m => PrimMonad (Op m) where
    type PrimState (Op m) = PrimState m
    primitive = Op . primitive

evalOpCount :: Monad m => Op m () -> m (OpCount Int)
evalOpCount m =
    counts <$> runReaderT (execStateT (unOp m) defaultOpState) defaultOpEnv

countOps :: (Typed a, Num (Exp a), MonadSpiral m)
         => SPL (Exp a)
         -> m (OpCount Int)
countOps e = do
    counts <- evalOpCount $
              extendVars [(vx, VarCE vx), (vy, VarCE vy)] $
              runSPL e (fromGather x) >>= computeP y
    resetUnique
    return counts
  where
    Z :. m :. n = splExtent e

    x = C (ix1 n) vx
    y = C (ix1 m) vy

    vx = fromString "X"
    vy = fromString "Y"

extendVars :: MonadSpiral m
           => [(Var, CExp)]
           -> Op m a
           -> Op m a
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
          -> Op m ()
insertVar v ce =
    modify $ \s -> s { vars = Map.insert v ce (vars s) }

lookupVar :: MonadSpiral m => Var -> Op m CExp
lookupVar v = do
    maybe_ce <- gets (Map.lookup v . vars)
    case maybe_ce of
      Nothing -> faildoc $ text "ZZ Unbound variable:" <+> ppr v
      Just ce -> return ce

-- | Cache a 'CExp'. This generates a local binding for the value of the 'CExp'.
cacheCExp :: MonadSpiral m => Type -> CExp -> Op m CExp
cacheCExp tau ce0 | shouldCacheCExp ce0 = do
    maybe_ce <- gets (Map.lookup ce0 . cexpCache)
    case maybe_ce of
      Just ce -> return ce
      Nothing -> go ce0
  where
    go (ComplexCE cr ci) | not useComplexType = do
        cr' <- cacheCExp DoubleT cr
        ci' <- cacheCExp DoubleT ci
        return $ ComplexCE cr' ci'

    go ce = do
        ctemp <- cgTemp tau
        -- cgAssign will modify the cache
        cgAssign tau ctemp ce
        return ctemp

cacheCExp _ ce =
    return ce

-- | @'insertCachedCExp' ce1 ce2@ adds @ce2@ as the cached version of @ce1@.
insertCachedCExp :: forall m . MonadSpiral m
                 => CExp
                 -> CExp
                 -> Op m ()
insertCachedCExp ce1 ce2 =
    modify $ \s -> s { cexpCache = Map.insert ce1 ce2 (cexpCache s) }

-- | Return 'True' if a 'CExp' is worth caching.
shouldCacheCExp :: CExp -> Bool
shouldCacheCExp ConstCE{}            = False
shouldCacheCExp (ComplexCE ce1 ce2)  = shouldCacheCExp ce1 || shouldCacheCExp ce2
shouldCacheCExp VarCE{}              = False
shouldCacheCExp (UnopCE Neg VarCE{}) = False
shouldCacheCExp _                    = True

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

-- | Count the number of operations in an expression.
opcount :: Monad m => CExp -> Op m ()
opcount ConstCE{} = return ()
opcount VarCE{}   = return ()

opcount (UnopCE op e) = do
    unop op
    opcount e

opcount (BinopCE op e1 e2) = do
    opcount e1
    opcount e2
    binop op

opcount (IdxCE _ es) =
    mapM_ opcount es

opcount (ComplexCE er ei) = do
    opcount er
    opcount ei

opcount (ReCE e) = opcount e
opcount (ImCE e) = opcount e

instance MonadSpiral m => MonadP (Op m) where
    -- | Always unroll loop in the continuation.
    alwaysUnroll = local $ \env -> env { unroll = True }

    -- | Should we unroll a loop of the given size?
    shouldUnroll n = do
        always <- asks unroll
        maxun  <- asksConfig maxUnroll
        return $ always || n <= maxun

    comment _ = return ()

    forP lo hi k = do
        should <- shouldUnroll (hi - lo)
        if should
          then mapM_ k [intE i | i <- [lo..hi-1::Int]]
          else do
            i     <- gensym "i"
            count <- collect_ $
                     extendVars [(i, VarCE i)] $
                     k (VarE i)
            tell (fmap (* (hi - lo)) count)

    tempP = do
        t <- gensym "t"
        insertVar t (VarCE t)
        return (VarE t)

    newArray sh = C sh <$> gensym "V"

    assignP :: forall a . Typed a => Exp a -> Exp a -> Op m ()
    assignP e1 e2 = do
        ce1 <- cgExp e1
        ce2 <- cgExp e2
        cgAssign tau ce1 ce2
      where
        tau :: Type
        tau = typeOf (undefined :: a)

    mustCache e = do
        ce <- cgCacheExp e
        t  <- gensymFromC "t" ce
        insertVar t ce
        return $ VarE t

    cacheArray a = C sh <$> gensym "K"
      where
        sh = extent a

gensymFromC :: MonadUnique m => String -> CExp -> m Var
gensymFromC _ (VarCE v) =
    return v

gensymFromC t _ =
    gensym t

cgConst :: Const a -> CConst
cgConst (IntC x)       = IntCC x
cgConst (IntegerC x)   = IntegerCC x
cgConst (DoubleC x)    = DoubleCC x
cgConst (RationalC x)  = RationalCC x
cgConst (ComplexC r i) = ComplexCC (cgConst r) (cgConst i)
cgConst (RouC x)       = RouCC x
cgConst (PiC x)        = PiCC x

-- | Compile an 'Exp a' and cache the result. If we try to compile the same
-- expression again, we will re-use the cached value.
cgCacheExp :: forall a m . Typed a => MonadSpiral m => Exp a -> Op m CExp
cgCacheExp e = cgExp e >>= cacheCExp (typeOf (undefined :: a))

cgExp :: MonadSpiral m => Exp a -> Op m CExp
cgExp (ConstE c)        = return $ ConstCE $ cgConst c
cgExp (VarE v)          = lookupVar v

cgExp (UnopE op e) =
    cgExp e >>= go op
  where
    go Neg ce    = return $ -ce
    go Abs ce    = return $ abs ce
    go Signum ce = return $ signum ce

cgExp (BinopE op e1 e2) = do
    ce1 <- cgExp e1
    ce2 <- cgExp e2
    go op ce1 ce2
  where
    go Add  ce1 ce2 = return $ ce1 + ce2
    go Sub  ce1 ce2 = return $ ce1 - ce2
    go Mul  ce1 ce2 = return $ ce1 * ce2
    go Quot ce1 ce2 = return $ ce1 `quot` ce2
    go Rem  ce1 ce2 = return $ ce1 `rem` ce2
    go Div  _   _   = fail "Can't happen"

cgExp (IdxE v es) =
    IdxCE v <$> mapM cgExp es

cgExp (ComplexE er ei) =
    ComplexCE <$> cgExp er <*> cgExp ei

cgExp (ReE e) =
    (fst . unComplex) <$> cgExp e

cgExp (ImE e) =
    (snd . unComplex) <$> cgExp e

unComplex :: CExp -> (CExp, CExp)
unComplex (ComplexCE r i) = (r, i)
unComplex ce              = (ReCE ce, ImCE ce)

cgAssign :: MonadSpiral m => Type -> CExp -> CExp -> Op m ()
cgAssign (ComplexT DoubleT) ce1 ce2 | not useComplexType = do
    opcount cr2
    opcount ci2
    insertCachedCExp cr2 cr1
    insertCachedCExp ci2 ci1
  where
    (cr1, ci1) = unComplex ce1
    (cr2, ci2) = unComplex ce2

cgAssign _ ce1 ce2 = do
    opcount ce2
    insertCachedCExp ce2 ce1

-- | Generate code to allocate a temporary value. A name for the temporary is
-- optionally provided.
cgTemp :: MonadSpiral m => Type -> Op m CExp
cgTemp (ComplexT DoubleT) | not useComplexType = do
    ct1 <- cgTemp DoubleT
    ct2 <- cgTemp DoubleT
    return $ ComplexCE ct1 ct2

cgTemp _tau =
    VarCE <$> gensym "t"

data CConst = IntCC      Int
            | IntegerCC  Integer
            | DoubleCC   Double
            | RationalCC Rational
            | ComplexCC  CConst CConst
            | RouCC      Rational
            | PiCC       Rational
  deriving (Eq, Ord, Show)

data CExp = ConstCE CConst
          | VarCE  Var
          | UnopCE Unop CExp
          | BinopCE Binop CExp CExp
          | IdxCE Var [CExp ]
          | ComplexCE CExp CExp
          | ReCE CExp
          | ImCE CExp
  deriving (Eq, Ord, Show)

instance Num CExp where
    e1 + (UnopCE Neg e2) = e1 - e2

    e1 + e2 = BinopCE Add e1 e2

    e1 - (UnopCE Neg e2) = e1 + e2

    e1 - e2 = BinopCE Sub e1 e2

    e1 * e2 = BinopCE Mul e1 e2

    negate (UnopCE Neg e) = e

    negate e = UnopCE Neg e

    abs = UnopCE Abs

    signum = UnopCE Signum

    fromInteger i = ConstCE (IntCC (fromIntegral i))

instance Enum CExp where
    toEnum n = ConstCE (IntCC (fromIntegral n))

    fromEnum (ConstCE (IntCC n)) = fromIntegral n
    fromEnum _                   = error "Enum CExp: fromEnum not implemented"

instance Real CExp where
    toRational (ConstCE (RationalCC n)) = toRational n
    toRational _                        = error "Real CExp: toRational not implemented"

instance Integral CExp where
    e1 `quot` e2 = BinopCE Quot e1 e2
    e1 `rem` e2  = BinopCE Rem e1 e2

    e1 `quotRem` e2 = (e1 `quot` e2, e1 `rem` e2)

    toInteger (ConstCE (IntCC i)) = fromIntegral i
    toInteger _                   = error "Integral CExp: toInteger not implemented"

instance Pretty CConst where
    ppr (IntCC x)      = ppr (IntC x)
    ppr (IntegerCC x)  = ppr (IntegerC x)
    ppr (DoubleCC x)   = ppr (DoubleC x)
    ppr (RationalCC x) = ppr (RationalC x)
    ppr (RouCC r)      = ppr (RouC r)
    ppr (PiCC r)       = ppr (PiC r)

    ppr (ComplexCC r i) = pprComplex r i

instance Pretty CExp where
    pprPrec p (ConstCE c) = pprPrec p c
    pprPrec p (VarCE v)   = pprPrec p v

    pprPrec p (UnopCE op e) =
        parensIf (p > precOf op) $
        ppr op <> pprPrec (precOf op) e

    pprPrec p (BinopCE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IdxCE ev eis) =
        ppr ev <> mconcat [brackets (ppr ei) | ei <- eis]

    pprPrec p (ComplexCE er ei) =
        parensIf (p > addPrec) $
        pprComplex er ei

    pprPrec p (ReCE e) =
        parensIf (p > appPrec) $
        text "re" <+> pprPrec appPrec1 e

    pprPrec p (ImCE e) =
        parensIf (p > appPrec) $
        text "im" <+> pprPrec appPrec1 e

pprComplex :: Pretty a => a -> a -> Doc
pprComplex r i = ppr r <+> text "+" <+> pprPrec appPrec1 i <> char 'i'
