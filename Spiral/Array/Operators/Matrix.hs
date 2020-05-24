{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Array.Operators.Matrix
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Operators.Matrix (
    row,
    rowS,

    col,
    colS,

    nrows,
    ncols,

    (<->),
    (<|>),
    block,
    submatrix,

    mXv,
    (#>),
    mXvP,

    transpose,
    inverse,
    mXm,
    kronecker,
    directSum
  ) where

import Prelude hiding ((!!))

import Control.Monad (forM_,
                      when)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Maybe
import Control.Monad.Primitive (PrimMonad,
                                PrimState)
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Array.Operators.Mapping
import Spiral.Array.Operators.Reduction
import Spiral.Exp
import Spiral.Monad
import Spiral.Program.Monad

-- | Extract a row from a matrix.
row :: IArray r DIM2 a
    => Matrix r a
    -> Int
    -> Vector D a
row a i = fromFunction (Z :. n) g
  where
    (Z :. _m :. n, f) = toFunction (delay a)

    g (Z :. j) = f (Z :. i :. j)

-- | Extract a symbolic row from a matrix.
rowS :: SArray r DIM2 a
     => Matrix r a
     -> Exp Int
     -> Vector DS a
rowS a i = fromSFunction (Z :. n) g
  where
    (Z :. _m :. n, f) = toSFunction (delayS a)

    g (Z :. j) = f (Z :. i :. j)

-- | Extract a column from a matrix.
col :: IArray r DIM2 a
    => Matrix r a
    -> Int
    -> Vector D a
col a j = fromFunction (Z :. m) g
  where
    (Z :. m :. _n, f) = toFunction (delay a)

    g (Z :. i) = f (Z :. i :. j)

-- | Extract a symbolic column from a matrix.
colS :: SArray r DIM2 a
     => Matrix r a
     -> Exp Int
     -> Vector DS a
colS a j = fromSFunction (Z :. m) g
  where
    (Z :. m :. _n, f) = toSFunction (delayS a)

    g (Z :. i) = f (Z :. i :. j)

-- | Number of rows
nrows :: IsArray r DIM2 a => Matrix r a -> Int
nrows a = m
  where
    Z :. m :. _n = extent a

-- | Number of columns
ncols :: IsArray r DIM2 a => Matrix r a -> Int
ncols a = n
  where
    Z :. _m :. n = extent a

-- | Vertical stacking of matrices
(<->) :: ( IArray r1 DIM2 a
         , IArray r2 DIM2 a
         )
      => Matrix r1 a
      -> Matrix r2 a
      -> Matrix D a
a <-> b
    | ncols b /= c = error "(<->): Incompatible matrix dimensions"
    | otherwise    = fromFunction (ix2 (r + nrows b) c) f
  where
    Z :. r :. c = extent a

    f (Z :. i :. j)
      | i < r     = a ! (i,j)
      | otherwise = b ! (i-r,j)

-- | Horizontal stacking of matrices
(<|>) :: ( IArray r1 DIM2 a
         , IArray r2 DIM2 a
         )
      => Matrix r1 a
      -> Matrix r2 a
      -> Matrix D a
a <|> b
    | nrows b /= r = error "(<|>): Incompatible matrix dimensions"
    | otherwise    = fromFunction (ix2 r (c + ncols b)) f
  where
    Z :. r :. c = extent a

    f (Z :. i :. j)
      | j < c     = a ! (i,j)
      | otherwise = b ! (i,j-c)

-- | Create a block matrix
block:: ( IArray r1 DIM2 a
        , IArray r2 DIM2 a
        , IArray r3 DIM2 a
        , IArray r4 DIM2 a
        )
     => Matrix r1 a
     -> Matrix r2 a
     -> Matrix r3 a
     -> Matrix r4 a
     -> Matrix D a
block tl tr bl br = (tl <|> tr) <-> (bl <|> br)

-- | Extract submatrix
submatrix :: IArray r DIM2 a
          => Int -- ^ Starting row
          -> Int -- ^ Number of rows
          -> Int -- ^ Starting column
          -> Int -- ^ Number columns
          -> Matrix r a
          -> Matrix D a
submatrix r rn c cn a =
    fromFunction (ix2 rn cn) f
  where
    f (Z :. i :. j) = a ! (r + i, c + j)

-- | Compute the matrix-vector product, @A x@.
mXv :: forall r1 r2 a .
       ( Num a
       , IArray r1 DIM2 a
       , IArray r2 DIM1 a
       )
    => Matrix r1 a -- ^ The matrix @A@
    -> Vector r2 a -- ^ The vector @x@
    -> Vector D  a
mXv a x
  | n' /= n   = errordoc $ text "mXv: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
  | otherwise = fromFunction (Z :. m) f
  where
    Z :. n'     = extent x'
    Z :. m :. n = extent a

    x' :: Vector M a
    x' = manifest x

    f :: DIM1 -> a
    f (Z :. i) = sum (map (t !) [0..n-1])
      where
        t :: Vector D a
        t = x' .* row a i

-- | Compute the matrix-vector product, @A x@.
infixr 8 #>
(#>) :: forall r1 r2 a .
        ( Num a
        , IArray r1 DIM2 a
        , IArray r2 DIM1 a
        )
     => Matrix r1 a -- ^ The matrix @A@
     -> Vector r2 a -- ^ The vector @x@
     -> Vector D  a
(#>) = mXv

-- | Transpose a matrix
transpose :: forall e r . IArray r DIM2 e
          => Array r DIM2 e
          -> Array D DIM2 e
transpose a = D (ix2 n m) f
  where
    Z :. m :. n = extent a

    f (Z :. i :. j) = index a (Z :. j :. i)

-- | A mutable matrix
data MMatrix s a = MMatrix DIM2 (MV.MVector s a)

-- | Read entry of MMatrix
readM :: PrimMonad m
      => MMatrix (PrimState m) a -- ^ Matrix
      -> (Int, Int)              -- ^ Index
      -> m a
readM (MMatrix sh mv) (i, j) = MV.read mv (toIndex sh (Z :. i :. j))

-- | Swap rows of a MMatrix
swapM :: PrimMonad m
      => MMatrix (PrimState m) a -- ^ Matrix
      -> Int                     -- ^ First row
      -> Int                     -- ^ Second row
      -> m ()
swapM (MMatrix sh@(Z :. _ :. n) mv) i j
  | i == j    = return ()
  | otherwise = forM_ [0..n-1] $ \k ->
                MV.swap mv (toIndex sh (Z :. i :. k)) (toIndex sh (Z :. j :. k))

-- | Multiply MMatrix row by a constant
multiplyM :: (Num a, PrimMonad m)
          => MMatrix (PrimState m) a -- ^ Matrix
          -> Int                     -- ^ Row
          -> a                       -- ^ Constant
          -> m ()
multiplyM (MMatrix sh@(Z :. _ :. n) mv) i k =
    forM_ [0..n-1] $ \j -> MV.modify mv (* k) (toIndex sh (Z :. i :. j))

-- | Add multiple of one MMatrix row to another
addM ::(Num a, PrimMonad m)
     => MMatrix (PrimState m) a -- ^ Matrix
     -> Int                     -- ^ Source row
     -> a                       -- ^ Multiplier
     -> Int                     -- ^ Destination row
     -> m ()
addM m@(MMatrix sh@(Z :. _ :. n) mv) i k i' =
    forM_ [0..n-1] $ \j -> do
        y <- (*) <$> pure k <*> readM m (i, j)
        MV.modify mv (+ y) (toIndex sh (Z :. i' :. j))

-- | Find row to serve as pivot for given column
pivotM :: (Eq a, Num a, PrimMonad m)
       => MMatrix (PrimState m) a -- ^ Matrix
       -> Int                     -- ^ Column to pivot on
       -> m Int
pivotM a@(MMatrix (Z :. m :. _) _) j = go j
  where
    go i | i == m = fail $ "Cannot find pivot: " Prelude.++ show i

    go i = do
        x <- readM a (i, j)
        if x /= 0
           then return i
           else go (i+1)

gaussianM :: forall a m . (Eq a, Fractional a, PrimMonad m)
          => MMatrix (PrimState m) a
          -> m ()
gaussianM a@(MMatrix (Z :. m :. _) _) =
    go 0
  where
    go :: Int -> m ()
    go i | i == m = return ()

    go i = do
        -- Find pivot row
        i' <- pivotM a i
        swapM a i i'
        -- Ensure leading constant is 1
        k <- readM a (i, i)
        multiplyM a i (recip k)

        -- Subtract
        forM_ [0..m-1] $ \i' ->
          if i == i'
          then return ()
          else do k <- readM a (i', i)
                  addM a i (- k) i'
        go (i+1)

gaussian :: forall a r m . (Eq a, Fractional a, MonadFail m, IArray r DIM2 a)
         => Matrix r a
         -> m (Matrix M a)
gaussian a =
    case go of
      Nothing -> fail "Non-invertable matrix"
      Just a' -> return a'
  where
    M sh v = manifest a

    go :: Maybe (Matrix M a)
    go = runST $ runMaybeT $ do
        mv <- V.thaw v
        gaussianM $ MMatrix sh mv
        M sh <$> V.unsafeFreeze mv

inverse :: forall a r m . (Eq a, Fractional a, MonadFail m, IArray r DIM2 a)
        => Matrix r a
        -> m (Matrix D a)
inverse a = do
    r <- gaussian (a <|> identity n)
    return $ submatrix 0 n n n r
  where
    Z :. n :. _ = extent a

-- | Compute the matrix-vector product, @y = A x@.
mXvP :: forall r1 r2 r3 a m .
        ( MonadSpiral m
        , Num (Exp a)
        , Typed a
        , IArray r1 DIM2 (Exp a)
        , SArray r2 DIM1 (Exp a)
        , MArray r3 DIM1 (Exp a)
        )
     => Matrix r1 (Exp a) -- ^ The matrix @A@
     -> Vector r2 (Exp a) -- ^ The vector @x@
     -> Vector r3 (Exp a) -- ^ The vector @y@
     -> P m ()
mXvP a x y = do
    when (n' /= n) $
      faildoc $ text "mXvP: mismatched dimensions in input. Expected" <+> ppr n <+> text "but got" <+> ppr n'
    shouldUnroll n >>= go y
  where
    Z :. n'     = extent x
    Z :. m :. n = extent a

    go :: Vector r3 (Exp a)
       -> Bool
       -> P m ()
    go y True =
        computeP y (fromSFunction (Z :. m) f)
      where
        f :: ExpShapeOf DIM1 -> Exp a
        f (Z :. i) = sum (map (t !!) [0..n-1])
          where
            -- XXX we know we are unrolling here, so we can coerce a to a
            -- symbolic array since we know we will only index it with integer
            -- constant indices.
            t = x ^* rowS (coerceSymbolic a) i

    go y _ = do
        a' <- cacheArray a
        forP 0 m $ \i -> do
            yi <- sumP $ x ^* rowS a' i
            write y (Z :. i) yi

-- | Kronecker product of two matrices.
kronecker :: forall r1 r2 a .
             ( Num a
             , IArray r1 DIM2 a
             , IArray r2 DIM2 a
             )
          => Matrix r1 a
          -> Matrix r2 a
          -> Matrix D  a
kronecker a b = fromFunction (ix2 (m*p) (n*q)) f
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j) = a ! ix2 (i `quot` p) (j `quot` q) *
                       b ! ix2 (i `rem` p)  (j `rem` q)

-- | Direct sum of two matrices.
directSum :: forall r1 r2 a .
             ( Num a
             , IArray r1 DIM2 a
             , IArray r2 DIM2 a
             )
          => Matrix r1 a
          -> Matrix r2 a
          -> Matrix D  a
directSum a b = fromFunction (ix2 (m+p) (n+q)) f
  where
    Z :. m :. n = extent a
    Z :. p :. q = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j)
        | i < m  && j < n  = a ! ix2 i j
        | i >= m && j >= n = b ! ix2 (i-m) (j-n)
        | otherwise        = 0

-- | Product of two matrices.
mXm :: forall r1 r2 a .
       ( Num a
       , IArray r1 DIM2 a
       , IArray r2 DIM2 a
       )
    => Matrix r1 a
    -> Matrix r2 a
    -> Matrix D  a
mXm a b
    | p /= p'   = error "mxM: matrix dimensions don't match"
    | otherwise = fromFunction (ix2 m n) f
  where
    Z :. m  :. p' = extent a
    Z :. p :. n   = extent b

    f :: DIM2 -> a
    f (Z :. i  :. j) = sum' $ row a i .* col b j

    sum' :: (Num a, IArray r DIM1 a) => Vector r a -> a
    sum' v = sum [v ! i | i <- [0..n-1]]
      where
          Z :. n = extent v
