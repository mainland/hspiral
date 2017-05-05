{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Spiral.Array.Repr.Hidden
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Array.Repr.Hidden (
    H,

    Array(..)
  ) where

import Prelude hiding (read)

import Text.PrettyPrint.Mainland.Class

import Spiral.Array
import Spiral.Exp

data H

instance IsArray H sh (Exp a) where
    data Array H sh (Exp a) = forall r . ( IsArray r sh (Exp a)
                                         , Pretty (Array r sh (Exp a))
                                         , IArray r sh (Exp a)
                                         , SArray r sh (Exp a)
                                         , MArray r sh (Exp a)
                                         , Computable r sh (Exp a)) => H (Array r sh (Exp a))

    extent (H arr) = extent arr

instance Pretty (Array H sh (Exp a)) where
    ppr (H arr) = ppr arr

instance Shape sh => IArray H sh (Exp a) where
    index (H arr) i = index arr i

instance SArray H sh (Exp a) where
    indexS (H arr) i = indexS arr i

instance MArray H sh (Exp a) where
    read  (H arr) i   = read arr i
    write (H arr) i e = write arr i e

instance Computable H sh (Exp a) where
    computeP a (H b) = computeP a b
