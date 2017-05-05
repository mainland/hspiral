{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Complex
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral
import Spiral.Config
import Spiral.Exp
import Spiral.SPL

main :: IO ()
main = defaultMain $ \_args ->
    genTransform (text "Page 35") $ (I 5 ⊗ F2) × Pi (L 10 5)

genTransform :: Doc -> SPL (Exp (Complex Double)) -> Spiral ()
genTransform doc t = do
    liftIO $ putDocLn $ doc </> ppr t
    useComplex <- asksConfig (testDynFlag UseComplex)
    if useComplex
      then codegenC "t" t
      else codegenC "t" (Re t)
