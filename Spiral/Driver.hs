{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Spiral.Driver
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Driver (
    Config(..),
    Spiral,
    runSpiral,
    runSpiralWith,

    defaultMainWith,
    defaultMain,

    parseOpts,
    usage,

    codegenC
  ) where

import Control.Monad.Exception (SomeException,
                                catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)
import qualified Data.Text.Lazy.Encoding as E
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode(..),
                  hClose,
                  hPrint,
                  hPutStrLn,
                  openFile,
                  stderr,
                  stdout)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Spiral.Backend.C
import Spiral.Config
import Spiral.Driver.Monad
import Spiral.Driver.Opts
import Spiral.Exp
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Uniq

defaultMain :: ([String] -> Spiral a) -> IO a
defaultMain = defaultMainWith mempty

defaultMainWith :: Config -> ([String] -> Spiral a) -> IO a
defaultMainWith config k = do
    args             <- getArgs
    (config', args') <- parseOpts args
    if mode config == Help
      then usage >>= hPutStrLn stderr >> exitFailure
      else runSpiral (localConfig (const (config <> config')) (k args')) `catch` printFailure
  where
    printFailure :: SomeException -> IO a
    printFailure e = hPrint stderr e >> exitFailure

-- | Generate C code for the given SPL transform.
codegenC :: (Typed a, Num (Exp a))
         => String
         -> SPL (Exp a)
         -> Spiral ()
codegenC fname e = do
    t <- toProgram fname e
    resetUnique
    defs <- evalCg $ cgProgram t
    writeOutput (toList defs)

writeOutput :: Pretty a
            => a
            -> Spiral ()
writeOutput x = do
    maybe_outpath <- asksConfig output
    linePragmas   <- asksConfig (testDynFlag LinePragmas)
    let pprint | linePragmas = prettyPragmaLazyText 80 . ppr
               | otherwise   = prettyLazyText 80 . ppr
    h <- case maybe_outpath of
           Nothing      -> return stdout
           Just outpath -> liftIO $ openFile outpath WriteMode
    liftIO $ B.hPut h $ E.encodeUtf8 (pprint x)
    liftIO $ hClose h
