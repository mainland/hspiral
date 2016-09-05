{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Spiral.Util.Name
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Util.Name where

import Data.String
import Data.Symbol
import Language.C.Quote (ToIdent(..))
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

import Spiral.Util.Uniq

data Name = Name
    { nameSym  :: !Symbol
    , nameUniq :: Maybe Uniq
    }
  deriving (Read, Show)

instance Eq Name where
    n1 == n2 = nameSym n1 == nameSym n2 && nameUniq n1 == nameUniq n2

instance Ord Name where
    compare n1 n2 =
        case compare (nameSym n1) (nameSym n2) of
          LT -> LT
          GT -> GT
          EQ -> compare (nameUniq n1) (nameUniq n2)

instance Pretty Name where
    ppr (Name sym u) = text (unintern sym) <> pprUniq u
      where
        pprUniq :: Maybe Uniq -> Doc
        pprUniq Nothing  = mempty
        pprUniq (Just u) = ppr u

instance IsString Name where
    fromString s = Name (intern s) Nothing

instance Gensym Name where
    gensymAt s _ = do
        u <- newUnique
        return $ Name (intern s) (Just u)

    uniquify (Name sym _) = do
        u <- newUnique
        return $ Name sym (Just u)

instance ToIdent Name where
    toIdent (Name sym Nothing)         = C.Id $ unintern sym
    toIdent (Name sym (Just (Uniq u))) = C.Id $ unintern sym ++ show u
