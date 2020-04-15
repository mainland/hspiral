{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Search.OpCount
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Search.OpCount (
    searchOpCount
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
import Spiral.Search
import Spiral.OpCount
import Spiral.RootOfUnity
import Spiral.SPL hiding ((<|>))
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

type SDFT m a = S Cache m a

-- | Search for the form of a transform with the best op-count.
searchOpCount :: (Typeable a, Typed a, Num (Exp a), MonadSpiral m)
              => SPL (Exp a)
              -> m (SPL (Exp a))
searchOpCount = runSearch mempty findDFT

lookupDFT :: (Typeable a, Ord a, Monad m)
          => Int
          -> a
          -> S Cache m (Maybe (SPL a, Metric))
lookupDFT n w = gets $ lookupT (n, w) . cache

cacheDFT :: (Typeable a, Num a, Ord a, Pretty a, MonadTrace m)
         => Int
         -> a
         -> SPL a
         -> Metric
         -> SDFT m ()
cacheDFT n w e m = do
    traceSearch $ text "Caching:" <+> ppr n <+> ppr w </> ppr e
    modify $ \s -> s { cache = insertT (n, w) (e, m) (cache s) }

findDFT :: forall a m . (Typeable a, Typed a, MonadSpiral m)
        => SPL (Exp a)
        -> SDFT m (SPL (Exp a))
findDFT (F n w) = do
   maybe_e <- lookupDFT n w
   case maybe_e of
     Just (e, _) -> return e
     Nothing     -> bestBreakdown n w

findDFT _ =
    mzero

-- | Find the best DFT breakdown.
bestBreakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
              => Int
              -> Exp a
              -> SDFT m (SPL (Exp a))
bestBreakdown n w = do
    useComplexType <- asksConfig $ testDynFlag UseComplex
    alts           <- observeAll (breakdown n w) >>= mapM (search findDFT)
    opcs           <- mapM (countOps' useComplexType tau) alts
    traceSearch $ text "DFT size" <+> ppr n <> text ":" <+> commasep [ppr (mulOps ops) <> char '/' <> ppr (addOps ops) | ops <- opcs]
    let (e, m) = minimumBy metricOrdering (alts `zip` opcs)
    cacheIfBetter n w e m
  where
    tau :: Type a
    tau = typeOf (undefined :: a)

    -- If we aren't using native complex numbers, we need to count operations on
    -- the version of the transform that takes a size-2n vector as input.
    countOps' :: (Typed b, RootOfUnity (Exp b))
              => Bool
              -> Type b
              -> SPL (Exp b)
              -> SDFT m (OpCount Int)
    countOps' False ComplexT{} = countOps . Re
    countOps' _     _          = countOps

-- | Generate DFT breakdowns
breakdown :: forall a m . (Typeable a, Typed a, RootOfUnity (Exp a), MonadSpiral m)
          => Int
          -> Exp a
          -> SDFT m (SPL (Exp a))
breakdown n w =
    bruteForce n w <|>
    cooleyTukeyBreakdowns n w <|>
    splitRadixBreakdown n w <|>
    splitRadix8Breakdown n w <|>
    improvedSplitRadixBreakdown n w <|>
    goodThomasBreakdowns n w <|>
    winogradSmallBreakdowns n w <|>
    winogradTwoBreakdowns n w <|>
    winogradLargeBreakdowns n w <|>
    winogradPowerBreakdowns n w <|>
    -- winogradSquareBreakdowns n w <|>
    raderBreakdowns n w

-- | Cache the given DFT transform if its metric improves on the previously
-- best-known DFT.
cacheIfBetter :: (Typeable a, Num (Exp a), MonadSpiral m)
              => Int
              -> Exp a
              -> SPL (Exp a)
              -> Metric
              -> SDFT m (SPL (Exp a))
cacheIfBetter n w e m = do
    maybe_e' <- lookupDFT n w
    case maybe_e' of
      Just t@(e', _) | metricOrdering t (e, m) /= LT -> return e'
      _ -> do cacheDFT n w e m
              return e

metricOrdering :: (a, Metric) -> (a, Metric) -> Ordering
metricOrdering (_, x) (_, y) =
    case compare (allOps x) (allOps y) of
      EQ -> compare (mulOps x) (mulOps y)
      o  -> o
