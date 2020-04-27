{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Spiral where

import Data.Complex.Cyclotomic (Cyclotomic)
import qualified Data.Text as T
import IHaskell.Display
import Text.LaTeX

import Spiral.Array
import Spiral.Exp
import Spiral.SPL
import Spiral.Util.Pretty.LaTeX

displayLaTeX :: Pretty a => a -> IO Display
displayLaTeX = display . IHaskell.Display.latex . T.unpack . render . math . ppr

instance IHaskellDisplay Cyclotomic where
  display = displayLaTeX

instance IHaskellDisplay (Const a) where
  display = displayLaTeX

instance IHaskellDisplay (Exp a) where
  display = displayLaTeX

instance (Pretty a, IArray r DIM2 a) => IHaskellDisplay (Matrix r a) where
  display = displayLaTeX

instance (Num a, Pretty a) => IHaskellDisplay (SPL a) where
  display = displayLaTeX
