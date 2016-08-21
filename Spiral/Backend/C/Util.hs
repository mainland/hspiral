{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Spiral.Backend.C.Util
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Backend.C.Util (
    ToInitializer(..),
    ToStm(..),
    ToBlockItems(..)
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Language.C.Quote as C
import Language.C.Quote.C

class ToInitializer a where
    toInitializer :: a -> C.Initializer

class ToStm a where
    toStm :: a -> C.Stm

    toStmList :: [a] -> C.Stm
    toStmList xs = [cstm|{ $stms:(map toStm xs) }|]

instance ToStm a => ToStm [a] where
    toStm = toStmList

instance ToStm C.BlockItem where
    toStm (C.BlockStm stm) = stm
    toStm item             = [cstm|{ $items:([item]) }|]

    toStmList [C.BlockStm stm] = stm
    toStmList items            = [cstm|{ $items:items }|]

class ToBlockItems a where
    toBlockItems :: a -> [C.BlockItem]

    toBlockItemsList :: [a] -> [C.BlockItem]
    toBlockItemsList = concatMap toBlockItems

instance ToBlockItems a => ToBlockItems [a] where
    toBlockItems = toBlockItemsList

instance ToBlockItems a => ToBlockItems (Seq a) where
    toBlockItems = toBlockItemsList . toList

instance ToBlockItems C.Stm where
    toBlockItems stm = [C.BlockStm stm]

    toBlockItemsList = map C.BlockStm

instance ToBlockItems C.InitGroup where
    toBlockItems decl = [C.BlockDecl decl]

    toBlockItemsList = map C.BlockDecl
