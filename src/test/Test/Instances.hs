{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Test.Instances
-- Copyright   :  (c) 2016-2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Instances () where

import Test.HUnit (Assertion)
import Test.Hspec.Core.Spec (Example(..),
                             Result)
import Test.QuickCheck (Property)

instance Example (IO Property) where
    type Arg (IO Property) = ()
    evaluateExample mp c action progressCallback = do
        p <- mp
        evaluateExample p c action progressCallback

instance Example (IO Assertion) where
    type Arg (IO Assertion) = ()
    evaluateExample ma c action progressCallback = do
        a <- ma
        evaluateExample a c action progressCallback

instance Example ((Property -> IO Result) -> IO Result) where
    type Arg ((Property -> IO Result) -> IO Result) = ()
    evaluateExample k c action progressCallback =
        k $ \p -> evaluateExample p c action progressCallback
