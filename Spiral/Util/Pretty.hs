-- |
-- Module      :  Spiral.Util.Pretty
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.Pretty (
    Fixity(..),
    Assoc(..),
    HasFixity(..),

    infix_,
    infixl_,
    infixr_,

    precOf,

    infixop,

    addPrec,
    addPrec1,
    mulPrec,
    mulPrec1
  ) where

import Text.PrettyPrint.Mainland

-- | Operator fixity.
data Fixity = Fixity Assoc Int
  deriving (Eq, Ord)

-- | Operator associativity.
data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Ord)

-- | Fixity for a non-fix operator.
infix_ :: Int -> Fixity
infix_ = Fixity NonAssoc

-- | Fixity for a left-associative operator.
infixl_ :: Int -> Fixity
infixl_ = Fixity LeftAssoc

-- | Fixity for a right-associative operator.
infixr_ :: Int -> Fixity
infixr_ = Fixity RightAssoc

-- | A type that has a fixity.
class HasFixity a where
    fixity :: a -> Fixity

-- | Return the precedence of a value with a fixity.
precOf :: HasFixity a => a -> Int
precOf op =
    p
  where
    Fixity _ p = fixity op

-- | Pretty-print an infix operator.
infixop :: (Pretty a, Pretty b, Pretty op, HasFixity op)
        => Int -- ^ precedence of context
        -> op  -- ^ operator
        -> a   -- ^ left argument
        -> b   -- ^ right argument
        -> Doc
infixop prec op l r =
    parensIf (prec > opPrec) $
    pprPrec leftPrec l <+> ppr op <+/> pprPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op

addPrec, addPrec1 :: Int
addPrec  = 8
addPrec1 = addPrec + 1

mulPrec, mulPrec1 :: Int
mulPrec  = 9
mulPrec1 = mulPrec + 1
