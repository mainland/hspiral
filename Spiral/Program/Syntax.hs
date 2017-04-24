{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Program.Syntax
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Program.Syntax (
    Program(..),
    Decl(..),
    Decls,
    Stm(..),
    Stms,
    Block(..)
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.PrettyPrint.Mainland hiding (flatten)

import Spiral.Array.Base
import Spiral.Array.Repr.Concrete
import Spiral.Array.Shape
import Spiral.Exp

data Program a = Program
    { name :: String
    , vin  :: Vector C (Exp a)
    , vout :: Vector C (Exp a)
    , code :: Block
    }

data Decl where
    VarD :: Var -> Type -> Decl
    ArrD :: Shape sh => Var -> sh -> Type -> Decl
    ConstArrD :: (Shape sh, Typed a, IArray r sh (Exp a)) => Var -> Array r sh (Exp a) -> Decl

type Decls = Seq Decl

data Stm where
    AssignS  :: Typed a => Exp a -> Exp a -> Stm
    CommentS :: Doc -> Stm
    ForS     :: Var -> Int -> Int -> Block -> Stm

type Stms = Seq Stm

data Block = Block Decls Stms

instance Monoid Block where
    mempty = Block mempty mempty

    Block d1 s1 `mappend` Block d2 s2 = Block (d1 <> d2) (s1 <> s2)

instance Typed a => Pretty (Program a) where
    ppr (Program f vin vout stms) =
        text f <> parens (commasep [ppr vin, ppr vout]) <+> ppr stms

instance Pretty Decl where
    ppr (VarD v tau) =
        ppr tau <+> ppr v <> semi

    ppr (ArrD v sh tau) =
        ppr tau <> ppr (listOfShape sh) <+> ppr v <> semi

    ppr (ConstArrD v (a :: Array r sh (Exp a))) =
        ppr tau <> ppr (listOfShape (extent a)) <+> ppr v <+> char '=' <+> ppr m <> semi
      where
        tau :: Type
        tau = typeOf (undefined :: a)

        m :: Array M sh (Exp a)
        m = manifest a

    pprList = embrace . map ppr

instance Pretty Stm where
    ppr (AssignS e1 e2) =
        ppr e1 <+> char '=' <+> ppr e2 <> semi

    ppr (CommentS doc) =
        text "//" <+> doc

    ppr (ForS i beg end block) =
        align $
        text "for" <+> ppr i <+> ppr beg <> text ".." <> ppr end <+> ppr block

    pprList [s] = ppr s
    pprList ss  = embrace (map ppr ss)

instance Pretty Stms where
    ppr = pprList . toList

instance Pretty Block where
    ppr (Block decls stms)
      | Seq.null decls = embrace $ map ppr (toList stms)
      | otherwise      = embrace $ map ppr (toList decls) ++ empty : map ppr (toList stms)

-- | Print a block of code surrounded by braces and separated by semicolons and
-- newlines. The opening brace appears on its own line, and all lines are nested
-- and aligned.
embrace :: [Doc] -> Doc
embrace ds =
    case ds of
      [] -> lbrace <> rbrace
      _  -> nest 2 (lbrace </> (align . folddoc (</>)) ds) </> rbrace
