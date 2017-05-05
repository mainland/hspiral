{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Array.Repr.Complex
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Complex (
    RE,
    toRealArray,

    CMPLX,
    toComplexArray,

    Array(..)
  ) where

import Prelude hiding (read)

import Data.Complex (Complex)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Exp

data RE r

toRealArray :: Array r (sh :. Int) (Exp (Complex a)) -> Array (RE r) (sh :. Int) (Exp a)
toRealArray = RE

instance IsArray r (sh :. Int) (Exp (Complex a)) => IsArray (RE r) (sh :. Int) (Exp a) where
    data Array (RE r) (sh :. Int) (Exp a) = RE (Array r (sh :. Int) (Exp (Complex a)))

    extent (RE a) = sh :. 2*n
      where
        sh :. n = extent a

instance Pretty (Array r (sh :. Int) (Exp (Complex a))) => Pretty (Array (RE r) (sh :. Int) (Exp a)) where
    ppr (RE a) = text "Re" <> parens (ppr a)

instance (Num (Exp a), Shape sh, IArray r (sh :. Int) (Exp (Complex a))) => IArray (RE r) (sh :. Int) (Exp a) where
    index (RE a) (sh :. i)
        | i `rem` 2 == 0 = re
        | otherwise      = im
      where
        (re, im) = unComplexE (index a (sh :. i `quot` 2))

instance (Num (Exp a), SArray r (sh :. Int) (Exp (Complex a))) => SArray (RE r) (sh :. Int) (Exp a) where
    indexS (RE a) (sh :. i)
        | i `rem` 2 == 0 = re
        | otherwise      = im
      where
        (re, im) = unComplexE (indexS a (sh :. i `quot` 2))

instance (Typed a, Num (Exp a), MArray r (sh :. Int) (Exp (Complex a))) => MArray (RE r) (sh :. Int) (Exp a) where
    read (RE a) (sh :. i) = do
        (re, im) <- unComplexE <$> read a (sh :. i `quot` 2)
        if i `rem` 2 == 0
          then return re
          else return im

    write (RE a) (sh :. i) e = do
        (re, im) <- unComplexE <$> read a (sh :. i `quot` 2)
        if i `rem` 2 == 0
          then write a (sh :. i `quot` 2) (ComplexE e im)
          else write a (sh :. i `quot` 2) (ComplexE re e)

instance (Num (Exp a), SArray r (sh :. Int) (Exp (Complex a))) => Computable (RE r) (sh :. Int) (Exp a) where
    computeP a b =
        forShapeP (extent b) $ \ix ->
            write a ix (indexS b ix)

data CMPLX r

toComplexArray :: Array r (sh :. Int) (Exp a) -> Array (CMPLX r) (sh :. Int) (Exp (Complex a))
toComplexArray = CMPLX

instance IsArray r (sh :. Int) (Exp a) => IsArray (CMPLX r) (sh :. Int) (Exp (Complex a)) where
    data Array (CMPLX r) (sh :. Int) (Exp (Complex a)) = CMPLX (Array r (sh :. Int) (Exp a))

    extent (CMPLX a) = sh :. n `quot` 2
      where
        sh :. n = extent a

instance Pretty (Array r (sh :. Int) (Exp a)) => Pretty (Array (CMPLX r) (sh :. Int) (Exp (Complex a))) where
    ppr (CMPLX a) = text "Cmplx" <> parens (ppr a)

instance (Typed a, Num (Exp a), IArray r (sh :. Int) (Exp a)) => IArray (CMPLX r) (sh :. Int) (Exp (Complex a)) where
    index (CMPLX a) (sh :. i) = ComplexE (index a (sh :. 2*i)) (index a (sh :. 2*i+1))

instance (Typed a, Num (Exp a), SArray r (sh :. Int) (Exp a)) => SArray (CMPLX r) (sh :. Int) (Exp (Complex a)) where
    indexS (CMPLX a) (sh :. i) = ComplexE (indexS a (sh :. 2*i)) (indexS a (sh :. 2*i+1))

instance (Typed a, Num (Exp a), MArray r (sh :. Int) (Exp a)) => MArray (CMPLX r) (sh :. Int) (Exp (Complex a)) where
    read (CMPLX a) (sh :. i) = ComplexE <$> read a (sh :. 2*i) <*> read a (sh :. 2*i+1)

    write (CMPLX a) (sh :. i) e = do
        write a (sh :. 2*i) er
        write a (sh :. 2*i+1) ei
      where
        (er, ei) = unComplexE e

instance (Typed a, Num (Exp a), Computable r (sh :. Int) (Exp a)) => Computable (CMPLX r) (sh :. Int) (Exp (Complex a)) where
    computeP a (CMPLX b) = computeP (toRealArray a) b
