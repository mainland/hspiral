-- |
-- Module      :  Spiral.Driver
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Driver (
    defaultMainWith,
    defaultMain,

    codegenC
  ) where

import Control.Monad.Exception (SomeException,
                                catch)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Complex
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

import Spiral
import Spiral.Backend.C
import Spiral.Config
import Spiral.Driver.Opts
import Spiral.Exp
import Spiral.SPL

defaultMain :: Spiral () -> IO ()
defaultMain = defaultMainWith mempty

defaultMainWith :: Config -> Spiral () -> IO ()
defaultMainWith config m = do
    args              <- getArgs
    (config', _files) <- parseOpts args
    if mode config == Help
      then usage >>= hPutStrLn stderr
      else runSpiral (localConfig (const (config <> config')) m) `catch` printFailure
  where
    printFailure :: SomeException -> IO ()
    printFailure e = hPrint stderr e >> exitFailure

codegenC :: String
         -> Matrix SPL (Exp (Complex Double))
         -> Spiral ()
codegenC name e = do
    defs <- evalCg $ codegen name e
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
