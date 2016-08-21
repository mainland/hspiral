-- |
-- Module      :  Spiral
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral (
    Spiral,
    runSpiral,

    codegen
  ) where

import Spiral.Monad (Spiral,
                     runSpiral)
import Spiral.Cg (codegen)
