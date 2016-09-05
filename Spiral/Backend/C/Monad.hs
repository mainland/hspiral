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
    MonadCg,

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
    insertCachedCExp,
    lookupCExp,

    extendVars,
    insertVar,
    lookupVar,

    cgType,
    cgArrayType,
    cgExp
  ) where

import Control.Monad.Exception (MonadException(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..),
                                RealWorld)
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
import Spiral.Array.Program
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Code
import Spiral.Backend.C.Util
import Spiral.Driver.Config
import Spiral.Driver.Globals
import Spiral.Driver.Monad (Spiral)
import Spiral.Exp
import Spiral.Util.Trace
import Spiral.Util.Uniq

class ( PrimMonad m
      , PrimState m ~ RealWorld
      , MonadRef IORef m
      , MonadConfig m
      , MonadUnique m
      , MonadTrace m
      ) => MonadCg m where

instance MonadCg Spiral where

data CgEnv = CgEnv { unroll :: Bool }

defaultCgEnv :: CgEnv
defaultCgEnv = CgEnv { unroll = False }

data CgState = CgState
    { -- | Generated code
      code :: Code
    , -- | Compiled variables
      vars :: Map Var CExp
    , -- | Cached compiler constants
      cinitCache :: Map C.Initializer C.Exp
    , -- | Cached compiled expressions
      cexpCache :: Map CExp CExp
    }

defaultCgState :: CgState
defaultCgState = CgState
    { code       = mempty
    , vars       = mempty
    , cinitCache = mempty
    , cexpCache  = mempty
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

extendVars :: MonadCg m
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

insertVar :: MonadCg m
          => Var
          -> CExp
          -> Cg m ()
insertVar v ce =
    modify $ \s -> s { vars = Map.insert v ce (vars s) }

lookupVar :: MonadCg m => Var -> Cg m CExp
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
      Nothing -> do ctemp  <- cgVar "m"
                    let ce =  [cexp|$id:ctemp|]
                    appendTopDecl [cdecl|$ty:ctau $id:ctemp = $init:cinits;|]
                    modify $ \s -> s { cinitCache = Map.insert cinits ce (cinitCache s) }
                    return ce

-- | Cache a 'CExp'. This generates a local binding for the value of the 'CExp'.
cacheCExp :: forall m . MonadCg m => Type -> CExp -> Cg m CExp
cacheCExp tau ce0 | shouldCacheCExp ce0 = do
    maybe_ce <- gets (Map.lookup ce0 . cexpCache)
    case maybe_ce of
      Just ce -> return ce
      Nothing -> go ce0
  where
    go (CComplex cr ci) | not useComplexType = do
        cr' <- cacheCExp DoubleT cr
        ci' <- cacheCExp DoubleT ci
        return $ CComplex cr' ci'

    go (CExp [cexp|$ce1 * $double:x - $ce2 * $double:y|]) | y < 0 =
        cacheCExp tau (CExp [cexp|$ce1 * $double:x + $ce2 * $double:(-y)|])

    go (CExp [cexp|$ce1 * $double:x + $ce2 * $double:y|]) | y < 0 =
        cacheCExp tau (CExp [cexp|$ce1 * $double:x - $ce2 * $double:(-y)|])

    go (CExp [cexp|$ce1 * $double:x + $ce2 * $double:y|]) | epsDiff x y = do
        ce12 <- cacheCExp tau e12
        cacheCExp tau (CExp [cexp|$double:x * $ce12|])
      where
        e12 :: CExp
        e12 = CExp [cexp|$ce1 + $ce2|]

    go (CExp [cexp|$ce1 * $double:x - $ce2 * $double:y|]) | epsDiff x y = do
        ce12 <- cacheCExp tau e12
        cacheCExp tau (CExp [cexp|$double:x * $ce12|])
      where
        e12 :: CExp
        e12 = CExp [cexp|$ce1 - $ce2|]

    go (CExp [cexp|-$ce1 - $ce2|]) = do
        ce1' <- lookupCExp (CExp [cexp|-$ce1|])
        ce2' <- lookupCExp (CExp [cexp|-$ce2|])
        case (ce1', ce2') of
          (CExp [cexp|$id:_|], CExp [cexp|$id:_|]) ->
              return $ CExp [cexp|$ce1' + $ce2'|]
          _ -> do
              ce' <- cacheCExp tau e'
              return $ CExp [cexp|-$ce'|]
      where
        e' :: CExp
        e' = CExp [cexp|$ce1 + $ce2|]

    go (CInit ini) = do
        ce <- cacheConst ini (cgType tau)
        return $ CExp ce

    go ce = do
        ctemp <- cgTemp tau Nothing
        -- cgAssign will modify the cache
        cgAssign tau ctemp ce
        return ctemp

    epsDiff :: forall a . (Ord a, Fractional a) => a -> a -> Bool
    epsDiff x y = abs (x - y) < eps
      where
        eps = 1e-15

cacheCExp _ ce =
    return ce

-- | Retunr 'True' if a 'CExp' is worth caching.
shouldCacheCExp :: CExp -> Bool
shouldCacheCExp CInt{}                = False
shouldCacheCExp CDouble{}             = False
shouldCacheCExp (CComplex ce1 ce2)    = shouldCacheCExp ce1 || shouldCacheCExp ce2
shouldCacheCExp (CExp [cexp|$id:_|])  = False
shouldCacheCExp (CExp [cexp|-$id:_|]) = False
shouldCacheCExp _                     = True

-- | @'insertCachedCExp' ce1 ce2@ adds @ce2@ as the cached version of @ce1@.
insertCachedCExp :: forall m . MonadCg m
                 => CExp
                 -> CExp
                 -> Cg m ()
insertCachedCExp ce1 ce2 =
    modify $ \s -> s { cexpCache = Map.insert ce1 ce2 (cexpCache s) }

-- | Look up the cached C expression corresponding to a 'CExp'. If the 'CExp'
-- has not been cached, we return it without caching it.
lookupCExp :: forall m . MonadCg m
           => CExp -> Cg m CExp
lookupCExp ce = do
    maybe_ce' <- gets (Map.lookup ce . cexpCache)
    case maybe_ce' of
      Just ce' -> return ce'
      Nothing  -> return ce

-- | Generate a unique C identifier name using the given prefix.
cgVar :: MonadUnique m => String -> Cg m C.Id
cgVar = gensym

-- | Generate code to allocate a temporary value. A name for the temporary is
-- optionally provided.
cgTemp :: MonadCg m => Type -> Maybe Var -> Cg m CExp
cgTemp (ComplexT DoubleT) _ | not useComplexType = do
    ct1 <- cgTemp DoubleT Nothing
    ct2 <- cgTemp DoubleT Nothing
    return $ CComplex ct1 ct2

cgTemp tau maybe_v = do
    ct <- case maybe_v of
            Nothing -> cgVar "t"
            Just v  -> return $ C.toIdent v noLoc
    appendDecl [cdecl|$ty:(cgType tau) $id:ct;|]
    return $ CExp [cexp|$id:ct|]

instance MonadCg m => MonadP (Cg m) where
    -- | Always unroll loop in the continuation.
    alwaysUnroll = local $ \env -> env { unroll = True }

    -- | Should we unroll a loop of the given size?
    shouldUnroll n = do
        always <- asks unroll
        maxun  <- asksConfig maxUnroll
        return $ always || n <= maxun

    comment doc =
        whenDynFlag GenComments $
            appendComment doc

    forP lo hi k = do
        should <- shouldUnroll (hi - lo)
        if should
          then mapM_ k [intE i | i <- [lo..hi-1::Int]]
          else do
            i <- gensym "i"
            items <- inNewBlock_ $
                     extendVars [(i, CExp [cexp|$id:i|])] $
                     k (VarE i)
            appendStm [cstm|for (int $id:i = $int:lo; $id:i < $int:hi; ++$id:i) $stm:(toStm items)|]

    tempP :: forall a . Typed a => Cg m (Exp a)
    tempP = do
        t  <- gensym "t"
        ct <- cgTemp tau (Just t)
        insertVar t ct
        return $ VarE t
      where
        tau :: Type
        tau = typeOf (undefined :: a)

    newArray :: forall sh a . (Shape sh, Typed a)
             => sh
             -> Cg m (Array C sh (Exp a))
    newArray sh = do
        v      <- gensym "v"
        let cv =  CExp [cexp|$id:v|]
        insertVar v cv
        appendDecl [cdecl|$ty:ctau $id:v;|]
        return $ C sh v
      where
        ctau :: C.Type
        ctau = cgArrayType (typeOf (undefined :: a)) sh

    assignP :: forall a . Typed a => Exp a -> Exp a -> Cg m ()
    assignP e1 e2 = do
        ce1 <- cgExp e1
        ce2 <- cgExp e2
        cgAssign tau ce1 ce2
      where
        tau :: Type
        tau = typeOf (undefined :: a)

    cache e@ConstE{} =
        return e

    cache e@VarE{} =
        return e

    cache e = do
        t  <- gensym "t"
        ce <- cgCacheExp e
        insertVar t ce
        return $ VarE t

    cacheArray a = do
        cinit <- arrayInit (manifest a)
        t     <- gensym "t"
        ct    <- cacheConst cinit [cty|static const $ty:ctau |]
        insertVar t (CExp ct)
        return $ C sh t
      where
        sh = extent a

        ctau :: C.Type
        ctau = cgArrayType (ComplexT DoubleT) sh

arrayInit :: forall a sh m . (Shape sh, Typed a, MonadCg m)
          => Array M sh (Exp a)
          -> Cg m C.Initializer
arrayInit a = do
    inits <- f (reverse (listOfShape (extent a))) []
    case inits of
      [init] -> return init
      _      -> return [cinit|{ $inits:inits }|]
  where
    f :: [Int] -> [Int] -> Cg m [C.Initializer]
    f [] ix = do
        ce <- cgExp (index a (shapeOfList ix))
        return $ cgElem ce
      where
        cgElem :: CExp -> [C.Initializer]
        cgElem (CComplex ce1 ce2) | not useComplexType =
            [toInitializer ce1, toInitializer ce2]

        cgElem ce =
            [toInitializer ce]

    f (n:ix) ix' = do
        inits <- mapM (\i -> f ix (i : ix')) [0..n-1]
        return [[cinit|{ $inits:(concat inits) }|]]

-- | Compile an assignment.
cgAssign :: MonadCg m => Type -> CExp -> CExp -> Cg m ()
cgAssign (ComplexT DoubleT) ce1 ce2 | not useComplexType = do
    insertCachedCExp cr2 cr1
    insertCachedCExp ci2 ci1
    appendStm [cstm|$cr1 = $cr2;|]
    appendStm [cstm|$ci1 = $ci2;|]
  where
    (cr1, ci1) = unComplex ce1
    (cr2, ci2) = unComplex ce2

cgAssign _ ce1 ce2 = do
    insertCachedCExp ce2 ce1
    appendStm [cstm|$ce1 = $ce2;|]

-- | Compiled a value of type 'Const a'.
cgConst :: Const a -> CExp
cgConst (IntC x)         = CInt x
cgConst (IntegerC x)     = CInt (fromIntegral x)
cgConst (RationalC x)    = CDouble x
cgConst (DoubleC x)      = CDouble (toRational x)
cgConst (ComplexC e1 e2) = CComplex (cgConst e1) (cgConst e2)
cgConst (PiC x)          = CDouble (toRational (fromRational x * pi :: Double))
cgConst e@RouC{}         = cgConst (flatten e)

-- | Compile an 'Exp a' and cache the result. If we try to compile the same
-- expression again, we will re-use the cached value.
cgCacheExp :: forall a m . Typed a => MonadCg m => Exp a -> Cg m CExp
cgCacheExp e = cgExp e >>= cacheCExp (typeOf (undefined :: a))

-- | Compile an 'Exp a'.
cgExp :: forall a m . (Typed a, MonadCg m) => Exp a -> Cg m CExp
cgExp (ConstE c) = return $ cgConst c
cgExp (VarE v)   = lookupVar v

cgExp (UnopE op e) =
    cgCacheExp e >>= go op
  where
    go Neg ce    = return $ -ce
    go Abs ce    = return $ abs ce
    go Signum ce = return $ signum ce

cgExp (BinopE op e1 e2) = do
    ce1 <- cgCacheExp e1
    ce2 <- cgCacheExp e2
    go op ce1 ce2
  where
    go Add  ce1 ce2 = return $ ce1 + ce2
    go Sub  ce1 ce2 = return $ ce1 - ce2
    go Mul  ce1 ce2 = return $ ce1 * ce2
    go Quot ce1 ce2 = return $ ce1 `quot` ce2
    go Rem  ce1 ce2 = return $ ce1 `rem` ce2
    go Div  _   _   = fail "Can't happen"

cgExp (IdxE v es) =
    case typeOf (undefined :: a) of
      ComplexT DoubleT | useComplexType -> mkIdx v es
      _                                 -> mkComplexIdx v es

cgExp (ComplexE er ei) =
    CComplex <$> cgExp er <*> cgExp ei

cgExp (ReE e) = do
    (cr, _ci) <- unComplex <$> cgExp e
    return cr

cgExp (ImE e) = do
    (_cr, ci) <- unComplex <$> cgExp e
    return ci

mkIdx :: MonadCg m => Var -> [Exp Int] -> Cg m CExp
mkIdx v es = do
    ces <- mapM cgCacheExp es
    return $ CExp $ foldr cidx [cexp|$id:v|] ces
  where
    cidx ci ce = [cexp|$ce[$ci]|]

mkComplexIdx :: MonadCg m => Var -> [Exp Int] -> Cg m CExp
mkComplexIdx cv []       = return $ CComplex (CExp [cexp|$id:cv[0]|]) (CExp [cexp|$id:cv[1]|])
mkComplexIdx cv (ci:cis) = CComplex <$> mkIdx cv (2*ci:cis) <*> mkIdx cv (2*ci+1:cis)

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
    cidx ctau i = [cty|$ty:ctau[static $int:i]|]
