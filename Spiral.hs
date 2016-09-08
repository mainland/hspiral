-- |
-- Module      :  Spiral
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral (
    Config(..),

    Spiral,
    runSpiral,
    runSpiralWith,

    defaultMainWith,
    defaultMain,

    parseOpts,

    codegenC
  ) where

import Spiral.Config (Config(..))
import Spiral.Driver (defaultMain,
                      defaultMainWith,
                      codegenC)
import Spiral.Driver.Monad (Spiral,
                            runSpiral,
                            runSpiralWith)
import Spiral.Driver.Opts (parseOpts)
