{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Test.Gen
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Gen (
    genComplexTransform,

    withLibltdl,
    withDL
  ) where

import Data.Complex
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Exception (bracket,
                          bracket_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)
import qualified Data.Text.Lazy.Encoding as E
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.LibLTDL
import Foreign.Ptr (FunPtr,
                    Ptr)
import System.IO (IOMode(..),
                  hClose,
                  openFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)
import Text.PrettyPrint.Mainland   hiding (flatten)

import qualified Spiral.Backend.C as C
import Spiral (Spiral,
               runSpiralWith)
import Spiral.Config
import Spiral.Exp
import Spiral.SPL

genComplexTransform :: Config
                    -> String
                    -> SPL (Exp (Complex Double))
                    -> IO (V.Vector (Complex Double) -> V.Vector (Complex Double))
genComplexTransform conf name e =
    withCompiledTransform conf name e $ \fptr ->
      return $ mkComplexTransform fptr

mkComplexTransform :: FunPtr (Ptr (Complex Double) -> Ptr (Complex Double) -> IO ())
                   -> V.Vector (Complex Double)
                   -> V.Vector (Complex Double)
mkComplexTransform fptr x = unsafePerformIO $ do
    my <- MV.new n
    let (fptr_x, _) = V.unsafeToForeignPtr0 x
    let (fptr_y, _) = MV.unsafeToForeignPtr0 my
    withForeignPtr fptr_x $ \ptr_x ->
      withForeignPtr fptr_y $ \ptr_y ->
        f ptr_x ptr_y
    V.freeze my
  where
    n = V.length x

    f :: Ptr (Complex Double) -> Ptr (Complex Double) -> IO ()
    f = dynComplexTransform fptr

foreign import ccall "dynamic"
    dynComplexTransform :: FunPtr (Ptr (Complex Double) -> Ptr (Complex Double) -> IO ())
                        -> Ptr (Complex Double)
                        -> Ptr (Complex Double)
                        -> IO ()

withCompiledTransform :: (Typed a, Num (Exp a), Num (Exp (Complex a)))
                      => Config
                      -> String
                      -> SPL (Exp (Complex a))
                      -> (FunPtr (Ptr (Complex a) -> Ptr (Complex a) -> IO ()) -> IO b)
                      -> IO b
withCompiledTransform conf fname e k = do
    runSpiralWith conf $ do
        c <- C.evalCg $ C.codegen fname (Re e)
        if True then writeOutput dotc (toList c) else return ()
    callProcess "gcc" ["-o", dotso, "-fPIC", "-shared", dotc]
    dlInit
    dlSetSearchPath ["."]
    h <- dlOpen (Just dotso)
    dlSym h fname >>= k
  where
    dotc, dotso :: FilePath
    dotc  = fname ++ ".c"
    dotso = fname ++ ".so"

withLibltdl :: SearchPath -> IO a -> IO a
withLibltdl path k = bracket_ dlInit dlExit (dlSetSearchPath path >> k)

withDL :: String -> (DLHandle -> IO a) -> IO a
withDL lib = bracket (dlOpen (Just lib)) dlClose

writeOutput :: Pretty a
            => FilePath
            -> a
            -> Spiral ()
writeOutput output x = do
    linePragmas <- asksConfig (testDynFlag LinePragmas)
    let pprint | linePragmas = prettyPragmaLazyText 80 . ppr
               | otherwise   = prettyLazyText 80 . ppr
    h <- liftIO $ openFile output WriteMode
    liftIO $ B.hPut h $ E.encodeUtf8 (pprint x)
    liftIO $ hClose h
