{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Util.Pretty.LaTeX
-- Copyright   :  (c) 2017-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.Pretty.LaTeX (
    Pretty(..)
  ) where

import Data.Complex (Complex(..))
import Data.Complex.Cyclotomic
import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Modular (Mod, unMod)
import Data.Monoid ((<>))
import Data.Ratio (Ratio, numerator, denominator)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Symbol (unintern)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.TypeLits (KnownNat)
import Text.LaTeX
import Text.LaTeX.Base.Class (comm0,
                              comm1)
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSMath

import Spiral.Array (IArray,
                     Matrix,
                     (!))
import qualified Spiral.Array as A
import Spiral.Array.Shape
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Name (Name(..))
import Spiral.Util.Pretty (Assoc(..),
                           Fixity(..),
                           HasFixity(..),
                           addPrec,
                           infixl_,
                           precOf)
import Spiral.Util.Uniq (Uniq(..))

class Pretty a where
  {-# MINIMAL pprPrec | ppr #-}
  ppr     :: a -> LaTeX
  pprPrec :: Int -> a -> LaTeX

  pprList     :: [a] -> LaTeX
  pprPrecList :: Int -> [a] -> LaTeX

  ppr       = pprPrec 0
  pprPrec _ = ppr

  pprPrecList _ = pprList
  pprList       = autoSquareBrackets . commasep . map ppr

parensIf :: Bool -> LaTeX -> LaTeX
parensIf False l = l
parensIf True l  = autoParens l

hsep :: LaTeX -> [LaTeX] -> LaTeX
hsep sep = mconcat . intersperse sep

commasep :: [LaTeX] -> LaTeX
commasep = hsep ","

mskip :: LaTeX -> LaTeX
mskip = comm1 "mskip"

nicefrac :: LaTeX -> LaTeX -> LaTeX
nicefrac e1 e2 = mempty ^: e1 <> (mskip "-2mu" <> "/" <> mskip "-1mu") !: e2
--nicefrac e1 e2 = e1 <> "/" <> e2

instance Pretty a => Pretty [a] where
  ppr     = pprList
  pprPrec = pprPrecList

instance Pretty a => Pretty (Set a) where
  ppr = autoBraces . commasep . map ppr . Set.toList

instance Pretty Bool where
  ppr True  = mathfrak "T"
  ppr False = mathfrak "F"

pprSignedIntegral :: (Integral a, Show a) => a -> LaTeX
pprSignedIntegral x
  | x < 0     = "-" <> pprSignedIntegral (-x)
  | otherwise = (fromString . show) x

pprRealFloat :: (RealFloat a, Show a) => a -> LaTeX
pprRealFloat x
    | isIntegral x = ppr (ceiling x :: Integer)
    | x < 0        = fromString $ showFloat (-x)
    | otherwise    = fromString $ showFloat x
  where
    isIntegral :: RealFrac a => a -> Bool
    isIntegral x = fromIntegral (ceiling x :: Integer) == x

instance Pretty Int where
  ppr = pprSignedIntegral

instance Pretty Integer where
  ppr = pprSignedIntegral

instance Pretty Float where
  ppr = pprRealFloat

instance Pretty Double where
  ppr = pprRealFloat

instance (Eq a, Num a, Pretty a) => Pretty (Ratio a) where
  ppr x
      | d == 1    = ppr n
      | otherwise = nicefrac (ppr n) (ppr d)
    where
      n = numerator x
      d = denominator x

-- | Pretty-print an imaginary number
pprIm :: (Eq a, Num a, Pretty a) => a -> LaTeX
pprIm 0    = mempty
pprIm 1    = "i"
pprIm (-1) = "-i"
pprIm i    = ppr i <> "i"

instance (Eq a, Num a, Pretty a) => Pretty (Complex a) where
  ppr (r :+ 0)    = ppr r
  ppr (0 :+ 1)    = "i"
  ppr (0 :+ (-1)) = "-i"
  ppr (0 :+ i)    = pprIm i
  ppr (r :+ i)    = case (T.unpack . render) (pprIm i) of
                      '-' : _ -> ppr r <> pprIm i
                      _ -> ppr r + pprIm i

instance Pretty Cyclotomic where
  pprPrec p (Cyclotomic n mp)
    | Map.null mp = "0"
    | null xs     = leadingTerm rat n ex
    | otherwise   = parensIf (p > addPrec) $
                    leadingTerm rat n ex <> mconcat (map (followingTerm n) xs)
    where
      ((ex,rat):xs) = Map.toList mp

      pprBaseExp :: Integer -> Integer -> LaTeX
      pprBaseExp n 1  = omega !: ppr n
      pprBaseExp n ex = omega !^ (ppr n, ppr ex)

      leadingTerm :: Rational -> Integer -> Integer -> LaTeX
      leadingTerm r _ 0 = ppr r
      leadingTerm r n ex
        | r == 1     = t
        | r == (-1)  = "-" <> t
        | r > 0      = ppr r <> t
        | r < 0      = "-" <> ppr (-r) <> t
        | otherwise  = mempty
        where
          t = pprBaseExp n ex

      followingTerm :: Integer -> (Integer, Rational) -> LaTeX
      followingTerm n (ex, r)
        | r == 1     = "+" <> t
        | r == (-1)  = "-" <> t
        | r > 0      = "+" <> ppr r <> t
        | r < 0      = "-" <> ppr (-r) <> t
        | otherwise  = mempty
        where
          t = pprBaseExp n ex

instance (Pretty i, KnownNat p) => Pretty (Mod i p) where
  ppr z = ppr (unMod z)

instance Pretty (Const a) where
  ppr (BoolC x)      = ppr x
  ppr (IntC x)       = ppr x
  ppr (IntegerC x)   = ppr x
  ppr (FloatC x)     = ppr x
  ppr (DoubleC x)    = ppr x
  ppr (RationalC x)  = ppr x
  ppr (ComplexC r i) = ppr (r :+ i)
  ppr (W _ 0 _)      = "1"
  ppr (W n 1 _)      = omega !: ppr n
  ppr (W n k _)      = omega !^ (ppr n, ppr k)
  ppr (CycC x)       = ppr x
  ppr (PiC k)        = ppr k <> pi_
  ppr (ModularC x)   = ppr x

instance Pretty Uniq where
  ppr (Uniq u) = ppr u

instance Pretty Name where
  ppr (Name sym Nothing)  = fromString (unintern sym)
  ppr (Name sym (Just u)) = fromString (unintern sym) !: ppr u

instance Pretty Var where
  ppr (Var n) = ppr n

instance (Pretty e, IArray r DIM2 e) => Pretty (Matrix r e) where
  ppr mat = bmatrix Nothing $ M.matrix m n f
    where
      Z :. m :. n = A.extent mat

      f :: (Int, Int) -> LaTeX
      f (i, j) = ppr (mat ! (i-1, j-1))

instance Pretty (Exp a) where
  pprPrec p (ConstE c) = pprPrec p c
  pprPrec p (VarE v)   = pprPrec p v

  pprPrec _ (UnopE Abs e) =
    autoBrackets "|" "|" (ppr e)

  pprPrec _ (UnopE Sqrt e) =
    tsqrt Nothing (ppr e)

  pprPrec p (UnopE op e) =
    unop p op e

  pprPrec p (BinopE op e1 e2) =
    infixop p op e1 e2

  pprPrec _ (IdxE ev eis) =
    ppr ev <> autoSquareBrackets (commasep (map ppr eis))

  pprPrec p (ComplexE er ei) =
    parensIf (p > addPrec) $
    ppr (er :+ ei)

  pprPrec _ (ReE e) =
    comm0 "Re" <> pprPrec 10 e

  pprPrec _ (ImE e) =
    comm0 "Im" <> pprPrec 10 e

  pprPrec p (BBinopE op e1 e2) =
    infixop p op e1 e2

  pprPrec p (IfE e1 e2 e3) =
    parensIf (p > 10) $
    operatorname (mathsf "if") <> ppr e1 <>
    operatorname (mathsf "then") <> ppr e2 <>
    operatorname (mathsf "else") <> ppr e3

instance Pretty Unop where
  ppr Neg    = "-"
  ppr Abs    = operatorname "abs"
  ppr Signum = operatorname "sgn"
  ppr Exp    = texp
  ppr Log    = tlog
  ppr Sqrt   = operatorname "sqrt"
  ppr Sin    = tsin
  ppr Cos    = tcos
  ppr Asin   = arcsin
  ppr Acos   = arccos
  ppr Atan   = arctan
  ppr Sinh   = tsinh
  ppr Cosh   = tcosh
  ppr Asinh  = operatorname "arsinh"
  ppr Acosh  = operatorname "arcosh"
  ppr Atanh  = operatorname "artanh"

instance Pretty Binop where
  ppr Add  = "+"
  ppr Sub  = "-"
  ppr Mul  = comm0 "cdot"
  ppr Quot = comm0 "quot"
  ppr Rem  = operatorname "rem"
  ppr Div  = operatorname "div"
  ppr Mod  = operatorname "mod"
  ppr FDiv = "/"

instance Pretty BBinop where
  ppr Eq = "="
  ppr Ne = comm0 "neq"
  ppr Lt = "<"
  ppr Le = comm0 "le"
  ppr Ge = comm0 "ge"
  ppr Gt = ">"

instance Pretty Permutation where
  ppr (L mn n)        =  "L" !^ (ppr n, ppr mn)
  ppr (J n)           = operatorname "J" !: ppr n
  ppr (CS n k)        = operatorname "S" !: ppr n <> autoParens (ppr k)
  ppr (CRT m n _ _)   = operatorname "CRT" !^ (ppr n, ppr m)
  ppr (CRT' m n _ _)  = operatorname "CRT'" !^ (ppr n, ppr m)
  ppr (Good m n _ _)  = operatorname "Good" !^ (ppr n, ppr m)
  ppr (Good' m n _ _) = operatorname "Good'" !^ (ppr n, ppr m)
  ppr (R p a)         = operatorname "R" !^ (ppr p, ppr a)
  ppr (Inv p _)       = ppr p ^: "-1"

data MatrixBinop = KOp
                 | DSOp
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
  fixity KOp  = infixl_ 7
  fixity DSOp = infixl_ 6

instance Pretty MatrixBinop where
  ppr KOp  = comm0 "otimes"
  ppr DSOp = comm0 "oplus"

pprArgs :: Pretty a => [a] -> LaTeX
pprArgs = autoParens . commasep . map ppr

instance (Num e, Pretty e) => Pretty (SPL e) where
  pprPrec p m@E{}       = pprPrec p (toMatrix m)
  pprPrec _ (I n)       = operatorname "I" !: ppr n
  pprPrec _ (T e)       = pprPrec 10 e ^: comm0 "intercal"
  pprPrec p (Pi pi)     = pprPrec p pi
  pprPrec _ (Rot alpha) = operatorname "R" !: ppr alpha
  pprPrec _ (Diag xs)   = operatorname "diag" <> pprArgs (V.toList xs)
  pprPrec p (KDiag _ e) = pprPrec p e

  pprPrec _ ((tl `Beside` tr) `Above` (bl `Beside` br)) =
    bmatrix Nothing $ M.matrix 2 2 f
      where
        f :: (Int, Int) -> LaTeX
        f (1, 1) = ppr tl
        f (1, 2) = ppr tr
        f (2, 1) = ppr bl
        f (2, 2) = ppr br
        f _      = error "can't happen"

  pprPrec _ (Above a b) =
    bmatrix Nothing $ M.matrix 2 1 f
      where
        f :: (Int, Int) -> LaTeX
        f (1, 1) = ppr a
        f (2, 1) = ppr b
        f _      = error "can't happen"

  pprPrec _ (Beside a b) =
    bmatrix Nothing $ M.matrix 1 2 f
      where
        f :: (Int, Int) -> LaTeX
        f (1, 1) = ppr a
        f (1, 2) = ppr b
        f _      = error "can't happen"

  pprPrec p (Kron a b)  = infixop p KOp a b
  pprPrec p (DSum a b)  = infixop p DSOp a b
  pprPrec _ (Prod a b)  = pprPrec 10 a <> pprPrec 10 b
  pprPrec _ (Circ xs)   = operatorname "circ" <> pprArgs (V.toList xs)
  pprPrec _ (Skew xs)   = operatorname "skew" <> pprArgs (V.toList xs)
  pprPrec _ (Toep xs)   = operatorname "toep" <> pprArgs (V.toList xs)
  pprPrec _ (Re a)      = comm0 "Re" <> autoParens (ppr a)
  pprPrec _ F2          = operatorname "F" !: "2"
  pprPrec _ (DFT n)     = operatorname "DFT" !: ppr n
  pprPrec _ (DFT' n)    = operatorname "DFT" !^ (ppr n, "-1")
  pprPrec _ (F n w)     = operatorname "DFT" !: ppr n <> autoParens (ppr w)
  pprPrec _ (F' n w)    = operatorname "DFT" !^ (ppr n, "-1") <> autoParens (ppr w)

unop :: (Pretty a, Pretty op, HasFixity op)
     => Int -- ^ precedence of context
     -> op  -- ^ operator
     -> a
     -> LaTeX
unop prec op x =
    parensIf (prec > precOf op) $
    ppr op <> pprPrec (precOf op) x

infixop :: (Pretty a, Pretty b, Pretty op, HasFixity op)
        => Int -- ^ precedence of context
        -> op  -- ^ operator
        -> a   -- ^ left argument
        -> b   -- ^ right argument
        -> LaTeX
infixop prec op l r =
    parensIf (prec > opPrec) $
    pprPrec leftPrec l <> ppr op <> pprPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op
