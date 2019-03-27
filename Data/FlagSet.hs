-- |
-- Module      :  Data.FlagSet
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Data.FlagSet (
    FlagSet,
    testFlag,
    setFlag,
    unsetFlag,

    fromList,
    toList
  ) where

import Data.Bits
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Word (Word32)

-- | A set of flags.
newtype FlagSet a = FlagSet Word32
  deriving (Eq, Ord)

instance Semigroup (FlagSet a) where
    FlagSet x <> FlagSet y = FlagSet (x .|. y)

instance Monoid (FlagSet a) where
    mempty = FlagSet 0

    mappend = (<>)

instance (Enum a, Bounded a, Show a) => Show (FlagSet a) where
    show = show . toList

testFlag :: Enum a => FlagSet a -> a -> Bool
testFlag (FlagSet fs) f = fs `testBit` fromEnum f

setFlag :: Enum a => FlagSet a -> a -> FlagSet a
setFlag (FlagSet fs) f = FlagSet $ fs `setBit` fromEnum f

unsetFlag :: Enum a => FlagSet a -> a -> FlagSet a
unsetFlag (FlagSet fs) f = FlagSet $ fs `clearBit` fromEnum f

toList :: Enum a => FlagSet a -> [a]
toList (FlagSet n) = [toEnum i | i <- [0..finiteBitSize n-1], n `testBit` i]

fromList :: Enum a => [a] -> FlagSet a
fromList fs = FlagSet $ foldl' (\x y -> setBit x (fromEnum y)) 0 fs
