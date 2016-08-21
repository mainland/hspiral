{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  SPL.Cg.Monad
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module SPL.Cg.Monad (
    CVec(..),
    MonadCg(..)
  ) where

import Data.Complex

import SPL.Exp
import SPL.Syntax

-- | Codegen's representation of a vector with stride and offset.
data CVec m = CVec
    { cvec    :: CExp m
    , cstride :: CExp m
    , coff    :: CExp m
    }

-- | Monad for SPL code generation. This allows us to abstract over the concrete
-- code generator.
class Monad m => MonadCg m where
    -- | Codegen's representation of a scalar expression.
    type CExp m

    -- | Set up code generator to compile a transform. The continuation is
    -- called with the input vector and output vector.
    cgTransform :: String                     -- ^ The name of the transform
                -> Ix                         -- ^ The dimensions of the transform
                -> (CVec m -> CVec m -> m ()) -- ^ The body of the transform
                -> m ()

    -- | Generate a temporary.
    cgTemp :: m (CExp m)

    -- | Generate code to index into a matrix.
    cgIdx :: SPL (Exp (Complex Double)) -- ^ Matrix
          -> (CExp m, CExp m)           -- ^ Index
          -> m (CExp m)

    -- | Generate code to index into a 'CVec m'.
    cgVIdx :: CVec m     -- ^ Vector
           -> CExp m     -- ^ Index
           -> m (CExp m)

    -- | Compile an 'Exp a'
    cgExp :: Exp a -> m (CExp m)

    -- | Compile an assignment.
    cgAssign :: CExp m -> CExp m -> m ()

    -- | Generate code for a loop with the given start and end.
    cgFor :: Int              -- ^ Initial value
          -> Int              -- ^ Upper bound (non-inclusive)
          -> (CExp m -> m ()) -- ^ Loop body
          -> m ()
