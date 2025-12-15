{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.OpCount
-- Copyright   :  (c) 2025 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.OpCountWHT (
    searchOpCountWHT
  ) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Control.Monad.State (gets,
                            modify)
import Data.Dynamic (Dynamic,
                     fromDynamic,
                     toDyn)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import qualified Data.Typeable as T
import Data.Typeable (Typeable,
                      TypeRep)
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.PrettyPrint.Mainland.Class

import Spiral.Config
import Spiral.Exp
import Spiral.Monad
import Spiral.OpCount
import Spiral.SPL hiding ((<|>))
import Spiral.Search
import Spiral.FFT.CooleyTukey (wht,
                               wht_iter)
import Spiral.Search.FFTBreakdowns
import Spiral.Util.Trace

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

instance Semigroup Cache where
    x <> y = Cache { cache = cache x `Map.union` cache y }

instance Monoid Cache where
    mempty = Cache mempty

    mappend = (<>)

type SWHT m a = S Cache m a

-- | Search for the form of a WHT transform with the best op-count.
searchOpCountWHT :: forall a m . (Typeable a, Typed a, Floating (Exp a), MonadSpiral m)
                 => SPL (Exp a)
                 -> m (SPL (Exp a))
searchOpCountWHT = runSearchWHT mempty findWHT

lookupWHT :: (Typeable a, Monad m)
          => Int
          -> S Cache m (Maybe (SPL a, Metric))
lookupWHT n = gets $ lookupT n . cache

cacheWHT :: forall a m . (Typeable a, Num a, Pretty a, MonadTrace m)
         => Int
         -> SPL a
         -> Metric
         -> SWHT m ()
cacheWHT n e m = do
    traceSearch $ text "Caching WHT:" <+> ppr n </> ppr e
    modify $ \s -> s { cache = insertT n (e, m) (cache s) }

findWHT :: forall a m . (Typeable a, Typed a, Floating (Exp a), MonadSpiral m)
        => SPL (Exp a)
        -> SWHT m (SPL (Exp a))
findWHT (WHT n) = do
    maybe_e <- lookupWHT n
    case maybe_e of
      Just (e, _) -> return e
      Nothing     -> bestBreakdown n

findWHT _ =
    mzero

-- | Find the best WHT breakdown.
bestBreakdown :: forall a m . (Typeable a, Typed a, Floating (Exp a), MonadSpiral m)
              => Int
              -> SWHT m (SPL (Exp a))
bestBreakdown n = do
    alts           <- observeAll (breakdown n) >>= mapM (searchWHT findWHT)
    opcs           <- mapM countOps alts
    traceSearch $ text "WHT size" <+> ppr n <> text ":" <+> commasep [ppr (mulOps ops) <> char '/' <> ppr (addOps ops) | ops <- opcs]
    let (e, m) = minimumBy metricOrdering (alts `zip` opcs)
    cacheIfBetter n e m
  where
    tau :: Type a
    tau = typeOf (undefined :: a)


-- | Generate WHT breakdowns.
breakdown :: forall a m . (Typeable a, Typed a, Floating (Exp a), MonadSpiral m)
          => Int
          -> SWHT m (SPL (Exp a))
breakdown n = whtBreakdowns n

-- | Cache the given WHT transform if its metric improves on the previously
-- best-known WHT.
cacheIfBetter :: (Typeable a, Num (Exp a), MonadSpiral m)
              => Int
              -> SPL (Exp a)
              -> Metric
              -> SWHT m (SPL (Exp a))
cacheIfBetter n e m = do
    maybe_e' <- lookupWHT n
    case maybe_e' of
      Just t@(e', _) | metricOrdering t (e, m) /= LT -> return e'
      _ -> do cacheWHT n e m
              return e

metricOrdering :: (a, Metric) -> (a, Metric) -> Ordering
metricOrdering (_, x) (_, y) =
    case compare (allOps x) (allOps y) of
      EQ -> compare (mulOps x) (mulOps y)
      o  -> o
