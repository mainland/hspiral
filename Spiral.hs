-- |
-- Module      :  Spiral
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral (
    Spiral,
    runSpiral,
    runSpiralWith,

    codegen
  ) where

import Spiral.Driver.Monad (Spiral,
                            runSpiral,
                            runSpiralWith)
import Spiral.Backend.C (codegen)
