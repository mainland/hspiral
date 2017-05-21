{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Search.Monad
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.Monad (
    S(..),
    runS,

    Metric,
    lookupDFT,
    cacheDFT
  ) where


import Control.Applicative (Alternative)
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logic (MonadLogic(..),)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.State (StateT(..),
                            evalStateT,
                            gets,
                            modify)
import Control.Monad.Trans (MonadTrans(..))
import Data.Dynamic
import Data.IORef (IORef)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Typeable as T
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Config
import Spiral.Monad
import Spiral.Search.SFKT
import Spiral.OpCount
import Spiral.SPL
import Spiral.Util.Trace
import Spiral.Util.Uniq

type Metric = OpCount Int

type TypeMap = Map TypeRep Dynamic

insertT :: (Typeable k, Typeable v, Ord k) => k -> v -> TypeMap -> TypeMap
insertT k v m =
    case Map.lookup tau m of
      Nothing -> Map.insert tau (toDyn $ Map.singleton k v) m
      Just m' -> case fromDynamic m' of
                   Nothing  -> error "Bad TypeMap!"
                   Just m'' -> Map.insert tau (toDyn $ Map.insert k v m'') m
  where
    tau :: TypeRep
    tau = T.typeOf k

lookupT :: (Typeable k, Typeable v, Ord k) => k -> TypeMap -> Maybe v
lookupT k m =
    case Map.lookup tau m of
      Nothing -> Nothing
      Just m' -> case fromDynamic m' of
                   Nothing  -> error "Bad TypeMap!"
                   Just m'' -> Map.lookup k m''
  where
    tau :: TypeRep
    tau = T.typeOf k

newtype Cache = Cache { cache :: TypeMap }

instance Monoid Cache where
    mempty = Cache mempty

    x `mappend` y = Cache { cache = cache x `Map.union` cache y }

newtype S m a = S { unS :: SFKT (StateT Cache m) a }
  deriving (Functor, Applicative, Monad,
            Alternative, MonadPlus,
            MonadIO,
            MonadLogic,
            MonadUnique,
            MonadConfig,
            MonadTrace)

instance MonadTrans S where
    lift m = S $ lift $ lift  m

deriving instance MonadRef IORef m => MonadRef IORef (S m)

instance PrimMonad m => PrimMonad (S m) where
    type PrimState (S m) = PrimState m
    primitive = S . primitive

instance MonadSpiral m => MonadSpiral (S m) where

runS :: forall m a . Monad m
     => S m a
     -> m a
runS m = evalStateT (runSFKT (unS m)) mempty

lookupDFT :: (Typeable a, Ord a, Monad m) => Int -> a -> S m (Maybe (SPL a, Metric))
lookupDFT n w = S $ lift $ gets $ lookupT (n, w) . cache

cacheDFT :: (Typeable a, Num a, Ord a, Pretty a, MonadTrace m)
         => Int
         -> a
         -> SPL a
         -> Metric
         -> S m ()
cacheDFT n w e m = do
    traceSearch $ text "Caching:" <+> ppr n <+> ppr w </> ppr e
    S $ lift $ modify $ \s -> s { cache = insertT (n, w) (e, m) (cache s) }
