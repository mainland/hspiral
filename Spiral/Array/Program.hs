{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Spiral.Array.Program
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Program (
    MonadP(..),

    C,
    Array(..),

    MArray(..),

    Compute(..),

    ForShape(..),

    (.:=.)
  ) where

import Prelude hiding ((!!), read)

import Data.IORef (IORef)
import Control.Monad.Primitive (PrimMonad(..),
                                RealWorld)
import Control.Monad.Ref (MonadRef(..))
import Text.PrettyPrint.Mainland

import Spiral.Array
import Spiral.Driver.Config
import Spiral.Exp
import Spiral.Util.Trace
import Spiral.Util.Uniq

-- | A monad that supports computation with array.
class ( PrimMonad m
      , PrimState m ~ RealWorld
      , MonadRef IORef m
      , MonadConfig m
      , MonadUnique m
      , MonadTrace m
      ) => MonadP m where
    -- | Always unroll loops in the given computation.
    alwaysUnroll :: m a -> m a

    -- | Return 'True' if loops of the given size should be unrolled.
    shouldUnroll :: Int -> m Bool

    -- | Comment on the code being generated.
    comment :: Doc -> m ()

    -- | @'forP' begin end k@ generates code for a fro loop with bounds @begin@
    -- and @end@, where the body of the loop is generated by calling the
    -- continuation @k@ with an expression representing the loop variable.
    forP :: Int -> Int -> (Exp Int -> m ()) -> m ()

    -- | Generate a temporary of the given type.
    tempP :: Typed a => m (Exp a)

    -- | Assign one expression to another.
    assignP :: Typed a => Exp a -> Exp a -> m ()

    -- | Cache the given expression. This serves as a hint that it will be used
    -- more than once.
    cache :: (Typed a, Num (Exp a)) => Exp a -> m (Exp a)
    cache e@ConstE{} =
        return e

    cache e@VarE{} =
        return e

    cache (ComplexE er ei) = do
        er' <- cache er
        ei' <- cache ei
        return (ComplexE er' ei')

    cache (BinopE Add e1 (UnopE Neg e2)) =
        cache (BinopE Sub e1 e2)

    cache (BinopE Add e1 (BinopE Mul c2 e2)) | isNeg c2 =
        cache (BinopE Sub e1 (BinopE Mul (-c2) e2))

    cache (BinopE Sub e1 (BinopE Mul c2 e2)) | isNeg c2 =
        cache (BinopE Add e1 (BinopE Mul (-c2) e2))

    cache (BinopE Add (BinopE Mul c1 e1) (BinopE Mul c2 e2))
      | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, epsDiff x1 x2 = do
        e12 <- cache (BinopE Add e1 e2)
        cache (BinopE Mul c1 e12)

    cache (BinopE Add (BinopE Mul c1 e1) (BinopE Mul c2 e2))
      | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, x1 < 0, epsDiff x1 (-x2) = do
        e12 <- cache (BinopE Sub e1 e2)
        cache (BinopE Mul c1 e12)

    cache (BinopE Sub (BinopE Mul c1 e1) (BinopE Mul c2 e2))
      | ConstE (DoubleC x1) <- c1, ConstE (DoubleC x2) <- c2, x1 < 0, epsDiff x1 (-x2) = do
        e12 <- cache (BinopE Add e1 e2)
        cache (BinopE Mul c1 e12)

    cache e =
        mustCache e

    -- | Really cache the expression.
    mustCache :: (Typed a, Num (Exp a)) => Exp a -> m (Exp a)

    -- | Create a new concrete array of the given shape.
    newArray :: (Shape sh, Typed a)
             => sh
             -> m (Array C sh (Exp a))

    -- | Cache the contents of a matrix, returning it as a concrete matrix.
    cacheArray :: (Typed a, Num (Exp a), IArray r sh (Exp a))
               => Array r sh (Exp a)
               -> m (Array C sh (Exp a))

isNeg :: Exp a -> Bool
isNeg (ConstE (DoubleC x)) = x < 0
isNeg _                    = False

epsDiff :: (Ord a, Fractional a) => a -> a -> Bool
epsDiff x y = abs (x - y) < eps
  where
    eps = 1e-15

-- | And alias for 'assignP'.
infix 4 .:=.
(.:=.) :: (MonadP m, Typed a) => Exp a -> Exp a -> m ()
(.:=.) = assignP

-- | A mutable array.
class IsArray r sh e => MArray r sh e where
    -- | Read an element of an array.
    read  :: MonadP m => Array r sh e -> ExpShapeOf sh -> m e

    -- | Write an element of an array.
    write :: MonadP m => Array r sh e -> ExpShapeOf sh -> e -> m ()

class IsArray r sh e => Compute r sh e where
    -- | Compute @a = b@, that is, compute an array's elements in the given
    -- destination.
    computeP :: ( ForShape sh
                , MArray r' sh e
                , MonadP m
                )
             => Array r' sh e -- ^ Destination
             -> Array r  sh e -- ^ Source
             -> m ()

instance Shape sh => Compute DS sh e where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)

-- | Type tag for a concrete array backed by storage in a variable.
data C

instance Shape sh => IsArray C sh e where
    -- | A matrix whose entries are stored in a concrete location.
    data Array C sh e = C sh Var

    extent (C sh _) = sh

instance Shape sh => IArray C sh (Exp e) where
    index (C _sh v) ix = IdxE v (map intE (listOfShape ix))

instance Shape sh => SArray C sh (Exp e) where
    indexS (C _sh v) ix = IdxE v (listOfExpShape ix)

instance (Shape sh, Typed a) => MArray C sh (Exp a) where
    read  (C _sh v) ix = return $ IdxE v (listOfExpShape ix)
    write (C _sh v) ix = assignP (IdxE v (listOfExpShape ix))

instance Shape sh => Compute C sh (Exp a) where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)

class Shape sh => ForShape sh where
    -- | Allow iterating over all indices in a shape.
    forShapeP :: MonadP m
              => sh
              -> (ExpShapeOf sh -> m ())
              -> m ()

instance ForShape Z where
    forShapeP _ k = k Z

instance ForShape sh => ForShape (sh :. Int) where
    forShapeP (sh :. i) k =
      forShapeP sh $ \csh ->
        forP 0 i $ \ci ->
          k (csh :. ci)
