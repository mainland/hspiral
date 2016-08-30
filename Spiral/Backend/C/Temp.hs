{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Spiral.Backend.C.Temp
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Temp (
    CTemp(..)
  ) where

import Data.Complex
import Language.C.Pretty ()
import qualified Language.C.Syntax as C
import Language.C.Quote.C

import Spiral.Array
import Spiral.Backend.C.Array
import Spiral.Backend.C.CExp
import Spiral.Backend.C.Monad
import Spiral.Backend.C.Types
import Spiral.Monad (MonadCg)
import Spiral.Shape

-- | Type-directed generation of temporary variables.
class CTemp a b | a -> b where
    -- | Generate a temporary variable.
    cgTemp :: MonadCg m => a -> Cg m b

cgRawTemp :: (ToCType a, MonadCg m) => a -> Cg m C.Exp
cgRawTemp a = do
    t <- cgVar "t"
    appendDecl [cdecl|$ty:ctau $id:t;|]
    return [cexp|$id:t|]
  where
    ctau :: C.Type
    ctau = toCType a

instance CTemp Int (CExp Int) where
    cgTemp x = CExp <$> cgRawTemp x

instance CTemp Double (CExp Double) where
    cgTemp x = CExp <$> cgRawTemp x

instance CTemp (Complex Double) (CExp (Complex Double)) where
    cgTemp x = CExp <$> cgRawTemp x

instance ( Shape sh
         , ToCType (Array r sh a)
         , IsArray r sh a
         , IsArray r sh (CExp a)
         )
      => CTemp (Array r sh a) (Array C sh (CExp a)) where
    cgTemp a = do
        ce <- cgRawTemp a
        return $ C (extent a) ce
