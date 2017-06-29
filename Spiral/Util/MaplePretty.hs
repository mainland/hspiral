{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Spiral.Util.MaplePretty
-- Copyright   :  (c) 2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.MaplePretty where

import Data.Ratio
import Data.Complex (Complex(..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Pretty (Assoc(..),
                           Fixity(..),
                           HasFixity(..),
                           infixl_,
                           precOf)

addPrec, addPrec1 :: Int
addPrec  = 8
addPrec1 = addPrec + 1

mulPrec, mulPrec1 :: Int
mulPrec  = 9
mulPrec1 = mulPrec + 1

class MaplePretty a where
    {-# MINIMAL pprmPrec | pprm #-}
    pprm     :: a -> Doc
    pprmPrec :: Int -> a -> Doc

    pprmList     :: [a] -> Doc
    pprmPrecList :: Int -> [a] -> Doc

    pprm        = pprmPrec 0
    pprmPrec _  = pprm

    pprmPrecList _  = pprmList
    pprmList xs     = list (map pprm xs)

instance MaplePretty a => MaplePretty [a] where
    pprm     = pprmList
    pprmPrec = pprmPrecList

instance MaplePretty Bool where
    pprm True  = text "true"
    pprm False = text "false"

instance MaplePretty Char where
    pprm = char

instance MaplePretty Int where
    pprm = int

instance MaplePretty Integer where
    pprm = integer

instance MaplePretty Float where
    pprm x | isIntegral = ppr (ceiling x :: Integer)
           | otherwise  = ppr x
      where
        x' :: Integer
        x' = ceiling x

        isIntegral :: Bool
        isIntegral = fromIntegral x' == x

instance MaplePretty Double where
    pprm x | isIntegral = ppr (ceiling x :: Integer)
           | otherwise  = ppr x
      where
        x' :: Integer
        x' = ceiling x

        isIntegral :: Bool
        isIntegral = fromIntegral x' == x

instance MaplePretty a => MaplePretty (Ratio a)  where
    pprm x = text "Fraction" <> parens (commasep [pprm (numerator x), pprm (denominator x)])

instance MaplePretty a => MaplePretty (Set a) where
    pprm xs = enclosesep lbrace rbrace comma (map pprm (Set.toList xs))

instance (MaplePretty e, IArray r DIM2 e) => MaplePretty (Matrix r e) where
      pprm m = text "Matrix" <> (parens . pprm . toLists) m

instance MaplePretty (Const a) where
    pprmPrec _ (BoolC x)     = pprm x
    pprmPrec _ (IntC x)      = pprm x
    pprmPrec _ (IntegerC x)  = pprm x
    pprmPrec _ (FloatC x)    = pprm x
    pprmPrec _ (DoubleC x)   = pprm x
    pprmPrec _ (RationalC x) = pprm x

    pprmPrec _ (ComplexC r i)
        | r == 0 && i == 0    = char '0'
        | r == 0 && i == 1    = char 'I'
        | r == 0 && i == (-1) = text "-I"
        | r == 0              = pprm i <> char '*' <> char 'I'
        | i == 0              = pprm r
        | otherwise           = text "Complex" <> pprArgs [pprm r, pprm i]

    pprmPrec _ (W _ 0 _) = text "1"
    pprmPrec _ (W n 1 _) = text "w_" <> ppr n
    pprmPrec _ (W n k _) = text "w_" <> ppr n <> char '^' <> ppr k

    pprmPrec p (CycC x) = text (showsPrec p x "")

instance MaplePretty Var where
    pprm = ppr

instance MaplePretty (Exp a) where
    pprmPrec p (ConstE c) = pprmPrec p c
    pprmPrec p (VarE v)   = pprmPrec p v

    pprmPrec p (UnopE op@Neg e) =
        unop p op e

    pprmPrec _ (UnopE op e) =
        ppr op <> parens (ppr e)

    pprmPrec _ (BinopE Quot e1 e2) =
        text "iquo" <> pprArgs [pprm e1, pprm e2]

    pprmPrec _ (BinopE Rem e1 e2) =
        text "irem" <> pprArgs [pprm e1, pprm e2]

    pprmPrec p (BinopE op e1 e2) =
        infixop p op e1 e2

    pprmPrec _ (IdxE ev eis) =
        pprm ev <> (brackets . commasep) [pprm (ix+1) | ix <- eis]

    pprmPrec p (ComplexE er ei) =
        parensIf (p > addPrec) $
        pprmComplex (er :+ ei)

    pprmPrec _ (ReE e) =
        text "Re" <> parens (pprm e)

    pprmPrec _ (ImE e) =
        text "Im" <> parens (pprm e)

    pprmPrec p (BBinopE op e1 e2) =
        infixop p op e1 e2

    pprmPrec _ (IfE e1 e2 e3) =
        text "if" <+> pprm e1 <+>
        text "then" <+> pprm e2 <+>
        text "else" <+> pprm e3 <+>
        text "end if"

instance MaplePretty Unop where
    pprm Neg    = char '-'
    pprm Abs    = text "abs"
    pprm Signum = text "signum"
    pprm Exp    = text "exp"
    pprm Log    = text "log"
    pprm Sin    = text "sin"
    pprm Cos    = text "cos"
    pprm Asin   = text "asin"
    pprm Acos   = text "acos"
    pprm Atan   = text "atan"
    pprm Sinh   = text "sinh"
    pprm Cosh   = text "cosh"
    pprm Asinh  = text "asinh"
    pprm Acosh  = text "acosh"
    pprm Atanh  = text "atanh"

instance MaplePretty Binop where
    pprm Add  = char '+'
    pprm Sub  = char '-'
    pprm Mul  = char '*'
    pprm Quot = text "`div`"
    pprm Rem  = text "`mod`"
    pprm Div  = char '/'

instance MaplePretty BBinop where
    pprm Eq = text "="
    pprm Ne = text "<>"
    pprm Lt = text "<"
    pprm Le = text "<="
    pprm Ge = text ">="
    pprm Gt = text ">"

instance (Num e, MaplePretty e) => MaplePretty (SPL e) where
    pprmPrec _ (I n)      = text "IdentityMatrix" <> parens (pprm n)
    pprmPrec _ (Diag xs)  = text "DiagonalMatrix" <> parens (list (map pprm (V.toList xs)))
    pprmPrec _ (Kron a b) = text "KroneckerProduct" <> pprArgs [pprm a, pprm b]
    pprmPrec _ (DSum a b) = text "DiagonalMatrix" <> parens (list [pprm a, pprm b])
    pprmPrec p (Prod a b) = infixop p POp a b
    --pprmPrec _ (Prod a b) = text "MatrixMatrixMultiply" <> pprArgs [pprm a, pprm b]
    pprmPrec p m          = pprmPrec p (toMatrix m)

data MatrixBinop = POp
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
    fixity POp = infixl_ 7

instance MaplePretty MatrixBinop where
    pprm POp = char '.'

pprArgs :: [Doc] -> Doc
pprArgs = parens . commasep

pprmComplex :: (Eq a, Num a, MaplePretty a) => Complex a -> Doc
pprmComplex (r :+ 0) = pprm r
pprmComplex (0 :+ i) = pprm i <> char '*' <> char 'I'
pprmComplex (r :+ i) = pprm r <+> char '+' <+> pprm i <> char '*' <> char 'I'

unop :: (MaplePretty a, MaplePretty op, HasFixity op)
     => Int -- ^ precedence of context
     -> op  -- ^ operator
     -> a
     -> Doc
unop prec op x =
    parensIf (prec > precOf op) $
    pprm op <> pprmPrec (precOf op) x

infixop :: (MaplePretty a, MaplePretty b, MaplePretty op, HasFixity op)
        => Int -- ^ precedence of context
        -> op  -- ^ operator
        -> a   -- ^ left argument
        -> b   -- ^ right argument
        -> Doc
infixop prec op l r =
    parensIf (prec > opPrec) $
    pprmPrec leftPrec l <+> pprm op <+/> pprmPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op
