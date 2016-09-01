{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

    ForShape(..),

    computeP,

    Assign(..),
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
    alwaysUnroll :: m a -> m a

    shouldUnroll :: Int -> m Bool

    comment :: Doc -> m ()

    tempP :: m (Exp a)

    -- | Create a new concrete array.
    newC :: Shape sh => sh -> m (Array C sh a)

    cacheMatrix :: Matrix r (Exp a) -> m (Matrix DS (Exp a))

    forP :: Int -> Int -> (Exp Int -> m ()) -> m ()

    assignP :: Exp a -> Exp a -> m ()

-- | A mutable array.
class IsArray r sh e => MArray r sh e where
    -- | Read an element of an array.
    read  :: MonadP m => Array r sh e -> ExpShapeOf sh -> m e

    -- | Write an element of an array.
    write :: MonadP m => Array r sh e -> ExpShapeOf sh -> e -> m ()

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

instance Shape sh => MArray C sh (Exp e) where
    read  (C _sh v) ix = return $ IdxE v (listOfExpShape ix)
    write (C _sh v) ix = assignP (IdxE v (listOfExpShape ix))

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

-- | Compute @a = b@, that is, compute an array's elements in the given
-- destination.
computeP :: ( ForShape sh
            , MArray r1 sh e
            , SArray r2 sh e
            , MonadP m
            )
         => Array r1 sh e -- ^ Destination
         -> Array r2 sh e -- ^ Source
         -> m ()
computeP a b =
    forShapeP (extent b) $ \ix ->
        write a ix (indexS b ix)

class Assign a b where
    -- | Overloaded assignment
    assign :: MonadP m => a -> b -> m ()

infix 4 .:=.
(.:=.) :: (Assign a b, MonadP m) => a -> b -> m ()
(.:=.) = assign

instance Assign (Exp a) (Exp a) where
    assign = assignP

instance ( ForShape sh
         , MArray r1 sh (Exp a)
         , SArray r2 sh (Exp a)
         )
         => Assign (Array r1 sh (Exp a)) (Array r2 sh (Exp a)) where
    assign = computeP
