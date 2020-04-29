{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Spiral.Util.Pretty.Maple
-- Copyright   :  (c) 2017-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.Pretty.Maple (
    Pretty(..)
  ) where

import Data.Ratio
import Data.Complex (Complex(..))
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import qualified Text.PrettyPrint.Mainland.Class as Pretty

import Spiral.Array
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Pretty (Assoc(..),
                           Fixity(..),
                           HasFixity(..),
                           addPrec,
                           mulPrec,
                           mulPrec1,
                           infixl_,
                           precOf)

class Pretty a where
    {-# MINIMAL pprPrec | ppr #-}
    ppr     :: a -> Doc
    pprPrec :: Int -> a -> Doc

    pprList     :: [a] -> Doc
    pprPrecList :: Int -> [a] -> Doc

    ppr        = pprPrec 0
    pprPrec _  = ppr

    pprPrecList _  = pprList
    pprList xs     = list (map ppr xs)

instance Pretty a => Pretty [a] where
    ppr     = pprList
    pprPrec = pprPrecList

instance Pretty Bool where
    ppr True  = text "true"
    ppr False = text "false"

instance Pretty Char where
    ppr = char

instance Pretty Int where
    ppr = int

instance Pretty Integer where
    ppr = integer

pprRealFrac :: (RealFrac a, Pretty.Pretty a) => a -> Doc
pprRealFrac x
    | isIntegral = Pretty.ppr (ceiling x :: Integer)
    | otherwise  = Pretty.ppr x
  where
    x' :: Integer
    x' = ceiling x

    isIntegral :: Bool
    isIntegral = fromIntegral x' == x

instance Pretty Float where
    ppr = pprRealFrac

instance Pretty Double where
    ppr = pprRealFrac

instance Pretty a => Pretty (Ratio a)  where
    ppr x = text "Fraction" <> parens (commasep [ppr (numerator x), ppr (denominator x)])

instance Pretty a => Pretty (Set a) where
    ppr xs = enclosesep lbrace rbrace comma (map ppr (Set.toList xs))

instance (Pretty e, IArray r DIM2 e) => Pretty (Matrix r e) where
    ppr m = text "Matrix" <> (parens . ppr . toLists) m

instance Pretty (Const a) where
    pprPrec _ (BoolC x)     = ppr x
    pprPrec _ (IntC x)      = ppr x
    pprPrec _ (IntegerC x)  = ppr x
    pprPrec _ (FloatC x)    = ppr x
    pprPrec _ (DoubleC x)   = ppr x
    pprPrec _ (RationalC x) = ppr x

    pprPrec _ (ComplexC r i)
        | r == 0 && i == 0    = char '0'
        | r == 0 && i == 1    = char 'I'
        | r == 0 && i == (-1) = text "-I"
        | r == 0              = ppr i <> char '*' <> char 'I'
        | i == 0              = ppr r
        | otherwise           = text "Complex" <> pprArgs [ppr r, ppr i]

    pprPrec _ (W _ 0 _) = text "1"
    pprPrec _ (W n 1 _) = text "w_" <> Pretty.ppr n
    pprPrec _ (W n k _) = text "w_" <> Pretty.ppr n <> char '^' <> Pretty.ppr k

    pprPrec p (CycC x) = text (showsPrec p x "")

    pprPrec p (PiC x) = parensIf (p > mulPrec) $
                        Pretty.pprPrec mulPrec1 x <> char '*' <> text "Pi"

    pprPrec p (ModularC x) = Pretty.pprPrec p x

instance Pretty Var where
    ppr = Pretty.ppr

instance Pretty (Exp a) where
    pprPrec p (ConstE c) = pprPrec p c
    pprPrec p (VarE v)   = pprPrec p v

    pprPrec p (UnopE op@Neg e) =
        unop p op e

    pprPrec _ (UnopE op e) =
        Pretty.ppr op <> parens (Pretty.ppr e)

    pprPrec _ (BinopE Quot e1 e2) =
        text "iquo" <> pprArgs [ppr e1, ppr e2]

    pprPrec _ (BinopE Rem e1 e2) =
        text "irem" <> pprArgs [ppr e1, ppr e2]

    pprPrec p (BinopE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IdxE ev eis) =
        ppr ev <> (brackets . commasep) [ppr (ix+1) | ix <- eis]

    pprPrec p (ComplexE er ei) =
        parensIf (p > addPrec) $
        pprComplex (er :+ ei)

    pprPrec _ (ReE e) =
        text "Re" <> parens (ppr e)

    pprPrec _ (ImE e) =
        text "Im" <> parens (ppr e)

    pprPrec p (BBinopE op e1 e2) =
        infixop p op e1 e2

    pprPrec _ (IfE e1 e2 e3) =
        text "if" <+> ppr e1 <+>
        text "then" <+> ppr e2 <+>
        text "else" <+> ppr e3 <+>
        text "end if"

instance Pretty Unop where
    ppr Neg    = char '-'
    ppr Abs    = text "abs"
    ppr Signum = text "signum"
    ppr Exp    = text "exp"
    ppr Log    = text "log"
    ppr Sqrt   = text "sqrt"
    ppr Sin    = text "sin"
    ppr Cos    = text "cos"
    ppr Asin   = text "asin"
    ppr Acos   = text "acos"
    ppr Atan   = text "atan"
    ppr Sinh   = text "sinh"
    ppr Cosh   = text "cosh"
    ppr Asinh  = text "asinh"
    ppr Acosh  = text "acosh"
    ppr Atanh  = text "atanh"

instance Pretty Binop where
    ppr Add  = char '+'
    ppr Sub  = char '-'
    ppr Mul  = char '*'
    ppr Quot = text "`div`"
    ppr Rem  = text "`mod`"
    ppr FDiv = char '/'

instance Pretty BBinop where
    ppr Eq = text "="
    ppr Ne = text "<>"
    ppr Lt = text "<"
    ppr Le = text "<="
    ppr Ge = text ">="
    ppr Gt = text ">"

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec _ (I n)      = text "IdentityMatrix" <> parens (ppr n)
    pprPrec _ (Diag xs)  = text "DiagonalMatrix" <> parens (list (map ppr (V.toList xs)))
    pprPrec _ (Kron a b) = text "KroneckerProduct" <> pprArgs [ppr a, ppr b]
    pprPrec _ (DSum a b) = text "DiagonalMatrix" <> parens (list [ppr a, ppr b])
    pprPrec p (Prod a b) = infixop p POp a b
    --pprPrec _ (Prod a b) = text "MatrixMatrixMultiply" <> pprArgs [ppr a, ppr b]
    pprPrec p m          = pprPrec p (toMatrix m)

data MatrixBinop = POp
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
    fixity POp = infixl_ 7

instance Pretty MatrixBinop where
    ppr POp = char '.'

pprArgs :: [Doc] -> Doc
pprArgs = parens . commasep

pprComplex :: (Eq a, Num a, Pretty a) => Complex a -> Doc
pprComplex (r :+ 0) = ppr r
pprComplex (0 :+ i) = ppr i <> char '*' <> char 'I'
pprComplex (r :+ i) = ppr r <+> char '+' <+> ppr i <> char '*' <> char 'I'

unop :: (Pretty a, Pretty op, HasFixity op)
     => Int -- ^ precedence of context
     -> op  -- ^ operator
     -> a
     -> Doc
unop prec op x =
    parensIf (prec > precOf op) $
    ppr op <> pprPrec (precOf op) x

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
