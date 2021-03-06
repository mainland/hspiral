{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.SPL
-- Copyright   :  (c) 2016-2017 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.SPL (
    module Spiral.Permutation,

    SPL(..),
    matrix,
    fromLists,
    fromFunction,

    diag,
    circ,
    skew,
    toep,
    permute,
    backpermute,

    extent,

    mXv,
    (#>),

    toMatrix,
    col,
    row,

    transpose,

    inverse,

    (<->),
    (<|>),
    block,

    (⊗),
    (×),
    (⊕),

    Z(..),
    (:.)(..),

    DIM0,
    DIM1,
    DIM2,
    ix1,
    ix2
  ) where

import Data.Complex
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.PrettyPrint.Mainland.Class

import qualified Spiral.Array as A
import Spiral.Array (D,
                     IArray,
                     M,
                     Matrix,
                     Vector,
                     manifest)
import qualified Spiral.Array.Operators.Mapping as A
import qualified Spiral.Array.Operators.Matrix as A
import Spiral.Array.Repr.Complex
import Spiral.Array.Shape
import Spiral.Exp
import Spiral.Permutation hiding (Inv)
import Spiral.RootOfUnity
import Spiral.Util.Pretty

-- | Wrap an array to ensure it is unconditionally Show-able.
newtype ShowArray r sh e = ShowArray { unShowArray :: Array r sh e }

instance (Show sh, Show e, IArray r sh e) => Show (ShowArray r sh e) where
    show arr = show (manifest (unShowArray arr))

-- | An SPL transform. The type index is the type of the scalar values in the
-- transformed vector.
data SPL a where
    -- | An "embedded" array with an unknown representation.
    E :: IArray r DIM2 e => ShowArray r DIM2 e -> SPL e

    -- | The $n \times n$ identity matrix.
    I :: Num e => Int -> SPL e

    -- | Transpose
    T :: SPL e -> SPL e

    -- | Inverse
    Inv :: (Eq e, Fractional e) => SPL e -> SPL e

    -- | A permutation
    Pi :: Permutation -> SPL e

    -- | The rotation matrix
    Rot :: Floating a => a -> SPL a

    -- | A diagonal matrix
    Diag :: V.Vector e -> SPL e

    -- | The $n \times n$ diagonal matrix with constant diagonal elements.
    KDiag :: Int -> e -> SPL e

    -- | Vertical stacking of transforms
    Above :: SPL e -> SPL e -> SPL e

    -- | Horizontal stacking of transforms
    Beside :: SPL e -> SPL e -> SPL e

    -- | Kronecker product
    Kron :: SPL e -> SPL e -> SPL e

    -- | Direct sum
    DSum :: SPL e -> SPL e -> SPL e

    -- | Matrix product
    Prod :: SPL e -> SPL e -> SPL e

    -- | Circulant matrix given first column
    Circ :: V.Vector e -> SPL e

    -- | Skew-Circulant matrix given first column
    Skew :: V.Vector e -> SPL e

    -- | Toeplitz matrix
    Toep :: V.Vector e -> SPL e

    -- | Make a complex transform into a real transform.
    Re :: RealFloatConst a => SPL (Exp (Complex a)) -> SPL (Exp a)

    -- | The 2x2 DFT
    F2 :: SPL e

    -- | The nxn DFT matrix
    DFT :: RootOfUnity a => Int -> SPL a

    -- | The nxn Inverse DFT matrix
    DFT' :: RootOfUnity a => Int -> SPL a

    -- | The nxn "rotated" DFT matrix
    F :: RootOfUnity a => Int -> a -> SPL a

    -- | The nxn "rotated" inverse DFT matrix
    F' :: RootOfUnity a => Int -> a -> SPL a

deriving instance Show e => Show (SPL e)
deriving instance Typeable e => Typeable (SPL e)

-- | Embed any 'Matrix' as an SPL term.
matrix :: IArray r DIM2 e
       => Array r DIM2 e
       -> SPL e
matrix = E . ShowArray

-- | Embed a matrix created from a list of lists of values.
fromLists :: [[e]] -> SPL e
fromLists = matrix . A.fromLists

-- | Embed a matrix created from a function mapping indices to elements.
fromFunction :: DIM2 -> (DIM2 -> e) -> SPL e
fromFunction sh = matrix . A.fromFunction sh

-- | Create a diagonal matrix
diag :: [a] -> SPL a
diag = Diag . V.fromList

-- | Create a circulant matrix from the first column
circ :: [a] -> SPL a
circ = Circ . V.fromList

-- | Create a skew circulant matrix from the first column
skew :: [a] -> SPL a
skew = Skew . V.fromList

-- | Create a Toeplitz matrix
toep :: [a] -> SPL a
toep = Toep . V.fromList

-- | Permute (scatter).
permute :: Permutation -> SPL e
permute = Pi

-- | Backpermute (gather).
backpermute :: Permutation -> SPL e
backpermute = Pi . invert

-- | Return the extent of an SPL transform.
extent :: SPL a -> DIM2
extent (E a)       = A.extent (unShowArray a)
extent (I n)       = ix2 n n
extent (T e)       = ix2 n m
                       where
                         Z :. m :. n = extent e
extent (Inv e)     = ix2 n n
                       where
                         Z :. n :. _ = extent e
extent (Pi p)      = ix2 (dim p) (dim p)
extent (Rot _)     = ix2 2 2
extent (Diag xs)   = ix2 n n
                       where
                         n = length xs
extent (KDiag n _) = ix2 n n

extent (Above a b) = ix2 (ra+rb) c
  where
    Z :. ra :. c  = extent a
    Z :. rb :. _c = extent b

extent (Beside a b) = ix2 r (ca+cb)
  where
    Z :. r  :. ca  = extent a
    Z :. _r :. cb = extent b

extent (Kron a b) = ix2 (m*p) (n*q)
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

extent (DSum a b) = ix2 (m+p) (n+q)
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

extent (Prod a b) = ix2 m q
  where
    Z :. m  :. _n = extent a
    Z :. _p :. q  = extent b

extent (Circ xs) = ix2 n n
  where
    n = length xs

extent (Skew xs) = ix2 n n
  where
    n = length xs

extent (Toep xs) = ix2 n n
  where
    n = (length xs + 1) `quot` 2

extent (Re a) = ix2 (2*m) (2*n)
  where
    Z :. m  :. n = extent a

extent F2 = ix2 2 2

extent (DFT n)  = ix2 n n
extent (DFT' n) = ix2 n n
extent (F n _)  = ix2 n n
extent (F' n _) = ix2 n n

-- | Transpose an SPL expression
transpose :: forall a . (Show a, Num a) => SPL a -> SPL a
-- transpose (E m)        = (matrix . A.transpose . unShowArray) m
transpose a@I{}        = a
transpose (T a)        = a
transpose (Inv a)      = Inv (transpose a)
transpose (Pi p)       = backpermute p
transpose (Rot alpha)  = Rot (-alpha)
transpose a@Diag{}     = a
transpose a@KDiag{}    = a
transpose (Above a b)  = Beside (transpose a) (transpose b)
transpose (Beside a b) = Above (transpose a) (transpose b)
transpose (Kron a b)   = Kron (transpose a) (transpose b)
transpose (DSum a b)   = DSum (transpose a) (transpose b)
transpose (Prod a b)   = Prod (transpose b) (transpose a)
transpose (Circ cs)    = circ (x:reverse xs)
                           where
                             x:xs = V.toList cs
transpose a@Skew{}     = a
transpose (Toep cs)    = Toep (V.reverse cs)
transpose (Re a)       = Re (transpose a)
transpose F2           = F2
transpose a@DFT{}      = a
transpose a@DFT'{}     = a
transpose a@F{}        = a
transpose a@F'{}       = a
transpose a            = T a

-- | Inverse of an SPL expression
inverse :: forall a . (Eq a, Fractional a) => SPL a -> SPL a
inverse a@I{}       = a
inverse (T a)       = T (inverse a)
inverse (Inv a)     = a
inverse (Pi p)      = backpermute p
inverse (Rot alpha) = Rot (-alpha)
inverse (Diag xs)   = Diag (fmap (1/) xs)
inverse (KDiag n k) = KDiag n (1/k)
inverse (Kron a b)  = Kron (inverse a) (inverse b)
inverse (Prod a b)  = Prod (inverse b) (inverse a)
inverse (Re a)      = Re (inverse a)
inverse F2          = F2
inverse (DFT n)     = DFT' n
inverse (DFT' n)    = DFT n
inverse (F n w)     = F' n w
inverse (F' n w)    = F n w
inverse a           = Inv a

-- | Compute the product, @A x@.
mXv :: forall r a .
       ( Num a
       , IArray r DIM1 a
       )
    => SPL a      -- ^ The SPL transform, @A@
    -> Vector r a -- ^ The vector, @x@
    -> Vector D a
mXv I{} x =
    A.delay x

mXv (Pi p) x =
    A.fromFunction (ix1 n) f
  where
    Z :. n = A.extent x

    f (Z :. i) = x A.! reindex i
      where
        reindex = toIdxMapping (invert p)

mXv (Diag d) x =
    A.fromVector d A..* x

mXv (KDiag n k) x =
    A.fromFunction (ix1 n) f
  where
    f (Z :. i) = k * x A.! i

mXv (Kron (I m) a) x =
    A.delay $
    A.concat [a #> A.slice (i*n') 1 n' x | i <- [0..m-1]]
  where
    Z :. _n :. n' = extent a

mXv (Kron a (I n)) x =
    permute (L (m*n) m) #> Kron (I n) a #> permute (L (m'*n) n) #> x
  where
    Z :. m :. m' = extent a

mXv (Kron a b) x =
    (I m ⊗ b) #> (a ⊗ I n') #> x
  where
    Z :. m :. _  = extent a
    Z :. _ :. n' = extent b

mXv (DSum a b) x =
    A.delay $
    (a #> A.slice 0 1 m x) A.++ (b #> A.slice m 1 n x)
  where
    Z :. _ :. m = extent a
    Z :. _ :. n = extent b

mXv (Prod a b) x =
    a #> b #> x

mXv a x =
    toMatrix a `A.mXv` x

-- | Compute the matrix-vector product, @A x@.
infixr 8 #>
(#>) :: forall r a .
        ( Num a
        , IArray r DIM1 a
        )
     => SPL a
     -> Vector r a
     -> Vector D a
(#>) = mXv

-- | Convert an SPL transform to an explicit matrix.
toMatrix :: forall e . Num e => SPL e -> Matrix M e
toMatrix (E a) =
    manifest (unShowArray a)

toMatrix (T e) = manifest $ A.transpose $ toMatrix e

toMatrix (Inv e) =
    case A.inverse $ toMatrix e of
      Nothing -> error "Matrix has no inverse"
      Just a  -> A.manifest a

toMatrix (Diag xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) | i == j    = xs V.! i
                    | otherwise = 0

toMatrix (KDiag n e) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = e
                    | otherwise = 0

toMatrix (Above a b) =
    A.manifest $ toMatrix a A.<-> toMatrix b

toMatrix (Beside a b) =
    A.manifest $ toMatrix a A.<|> toMatrix b

toMatrix (Kron a b) =
    A.manifest $ A.kronecker (toMatrix a) (toMatrix b)

toMatrix (DSum a b) =
    A.manifest $ A.directSum (toMatrix a) (toMatrix b)

toMatrix (Prod a b) =
    A.manifest $ A.mXm (toMatrix a) (toMatrix b)

toMatrix (Circ xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) = xs V.! ((i-j) `mod` n)

toMatrix (Skew xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = length xs

    f (Z :. i :. j) = xs V.! ((i+j) `mod` n)

toMatrix (Toep xs) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    n = (length xs + 1) `quot` 2

    f (Z :. i :. j) = xs V.! (i-j+n-1)

toMatrix (I n) =
    manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) | i == j    = 1
                    | otherwise = 0

toMatrix (Rot alpha) =
    A.matrix [[cos alpha, -(sin alpha)],
              [sin alpha, cos alpha]]

toMatrix (Pi p) =
    manifest $ A.fromFunction (ix2 (dim p) (dim p)) f
  where
    f (Z :. i :. j) | g j == i  = 1
                    | otherwise = 0

    g = toIdxMapping p

toMatrix (Re a) = manifest (RE (toMatrix a))

toMatrix F2 =
    A.matrix [[1,  1],
              [1, -1]]

toMatrix (DFT n)  = toMatrix (F n (omega n))
toMatrix (DFT' n) = toMatrix (F' n (omega n))

toMatrix (F n w) = manifest $ A.fromFunction (ix2 n n) f
  where
    f (Z :. i :. j) = w ^ (i*j)

toMatrix (F' n w) = toMatrix (KDiag n (1/fromIntegral n) × F n (1/w))

pprArgs :: Pretty a => [a] -> Doc
pprArgs = parens . commasep . map ppr

instance (Num e, Pretty e) => Pretty (SPL e) where
    pprPrec p (E a)        = pprPrec p (manifest (unShowArray a))
    pprPrec _ (I n)        = text "I_" <> ppr n
    pprPrec _ (T e)        = pprPrec 10 e <> text "^T"
    pprPrec _ (Inv e)      = pprPrec 10 e <> text "^-1"
    pprPrec _ (Pi p)       = ppr p
    pprPrec _ (Rot alpha)  = text "R_" <> ppr alpha
    pprPrec _ (Diag xs)    = text "diag" <> pprArgs (V.toList xs)
    pprPrec _ (KDiag n e)  = text "kdiag" <> parens (commasep [ppr n, ppr e])
    pprPrec p (Above a b)  = infixop p AboveOp a b
    pprPrec p (Beside a b) = infixop p BesideOp a b
    pprPrec p (Kron a b)   = infixop p KOp a b
    pprPrec p (DSum a b)   = infixop p DSOp a b
    pprPrec p (Prod a b)   = infixop p POp a b
    pprPrec _ (Circ xs)    = text "circ" <> pprArgs (V.toList xs)
    pprPrec _ (Skew xs)    = text "skew" <> pprArgs (V.toList xs)
    pprPrec _ (Toep xs)    = text "toep" <> pprArgs (V.toList xs)
    pprPrec _ (Re a)       = text "Re" <> parens (ppr a)
    pprPrec _ F2           = text "F_2"
    pprPrec _ (DFT n)      = text "DFT_" <> ppr n
    pprPrec _ (DFT' n)     = text "DFT'_" <> ppr n
    pprPrec _ (F n w)      = text "F_" <> ppr n <> parens (ppr w)
    pprPrec _ (F' n w)     = text "F'_" <> ppr n <> parens (ppr w)

data MatrixBinop = AboveOp
                 | BesideOp
                 | KOp
                 | DSOp
                 | POp
  deriving (Eq, Ord, Show)

instance HasFixity MatrixBinop where
    fixity AboveOp  = infixl_ 7
    fixity BesideOp = infixl_ 7
    fixity KOp      = infixl_ 7
    fixity DSOp     = infixl_ 6
    fixity POp      = infixl_ 7

instance Pretty MatrixBinop where
    ppr AboveOp  = text "<->"
    ppr BesideOp = text "<|>"
    ppr KOp      = char '⊗'
    ppr DSOp     = char '⊕'
    ppr POp      = char '×'

-- | Extract a row of a matrix
row :: Matrix M e -> Int -> V.Vector e
row e i = V.slice (i*n) n xs
  where
    M (Z :. _m :. n) xs = manifest e

-- | Extract a column of a matrix
col :: forall e . Matrix M e -> Int -> V.Vector e
col e j = V.generate m f
  where
    M (Z :. m :. n) xs = manifest e

    f :: Int -> e
    f i = xs V.! (i*n + j)

-- | Vertical stacking of transforms
infixr 7 <->
(<->) :: SPL e -> SPL e -> SPL e
(<->) = Above

-- | Horizontal stacking of transforms
infixr 7 <|>
(<|>) :: SPL e -> SPL e -> SPL e
(<|>) = Beside

-- | Create a block matrix
block :: SPL e -> SPL e -> SPL e -> SPL e -> SPL e
block tl tr bl br = (tl <|> tr) <-> (bl <|> br)

-- | Alias for Kronecker product.
infixl 7 ⊗
(⊗) :: SPL e -> SPL e -> SPL e
I m ⊗ (Kron (I n) y) = I (m*n) ⊗ y
I 1 ⊗ y              = y
x   ⊗ I 1            = x
x   ⊗ y              = Kron x y

-- | Alias for matrix direct sum.
infixl 6 ⊕
(⊕) :: SPL e -> SPL e -> SPL e
I m     ⊕ I n     = I (n + m)
Diag xs ⊕ Diag ys = Diag (xs <> ys)
x       ⊕ y       = DSum x y

-- | Alias for matrix product.
infixl 7 ×
(×) :: SPL e -> SPL e -> SPL e
(×) = Prod
