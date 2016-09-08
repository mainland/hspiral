-- |
-- Module      :  Test.FFTW
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.FFTW (
    fft
  ) where

import Data.Complex
import qualified Data.Vector.Storable as V
import Numeric.FFT.Vector.Unnormalized as FFTW

fft :: Int -> V.Vector (Complex Double) -> V.Vector (Complex Double)
fft n = FFTW.execute $ FFTW.plan FFTW.dft n
