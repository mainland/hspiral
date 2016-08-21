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

    infixop
  ) where

import Text.PrettyPrint.Mainland

data Fixity = Fixity Assoc Int
  deriving (Eq, Ord)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Ord)

infix_ :: Int -> Fixity
infix_ = Fixity NonAssoc

infixl_ :: Int -> Fixity
infixl_ = Fixity LeftAssoc

infixr_ :: Int -> Fixity
infixr_ = Fixity RightAssoc

class HasFixity a where
    fixity :: a -> Fixity

precOf :: HasFixity a => a -> Int
precOf op =
    p
  where
    Fixity _ p = fixity op

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
