-- |
-- Module      :  Spiral
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral (
    Spiral,
    runSpiral,

    cgSPL
  ) where

import Spiral.Monad (Spiral,
                     runSpiral)
import Spiral.Cg (cgSPL)
