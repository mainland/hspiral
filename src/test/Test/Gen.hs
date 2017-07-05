{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Test.Gen
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Test.Gen (
    genComplexTransform,
    genModularTransform,

    withLibltdl,
    withDL
  ) where

import Data.Complex
import Data.Modular
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Exception (bracket,
                          bracket_)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)
import qualified Data.Text.Lazy.Encoding as E
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.LibLTDL
import Foreign.Ptr (FunPtr,
                    Ptr)
import Foreign.Storable (Storable)
import GHC.TypeLits (KnownNat)
import System.IO (IOMode(..),
                  hClose,
                  openFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import qualified Spiral.Backend.C as C
import Spiral (Spiral,
               runSpiralWith)
import Spiral.Config
import Spiral.Exp
import Spiral.SPL
import Spiral.SPL.Run

genComplexTransform :: Config
                    -> String
                    -> SPL (Exp (Complex Double))
                    -> IO (V.Vector (Complex Double) -> V.Vector (Complex Double))
genComplexTransform conf name e =
    withCompiledTransform conf name (Re e) $ \fptr ->
      return $ mkTransform (dynComplexTransform fptr)

foreign import ccall "dynamic"
    dynComplexTransform :: FunPtr (Ptr (Complex Double) -> Ptr (Complex Double) -> IO ())
                        -> Ptr (Complex Double)
                        -> Ptr (Complex Double)
                        -> IO ()

genModularTransform :: KnownNat p
                    => Config
                    -> String
                    -> SPL (Exp (ℤ/p))
                    -> IO (V.Vector (ℤ/p) -> V.Vector (ℤ/p))
genModularTransform conf name e =
    withCompiledTransform conf name e $ \fptr ->
      return $ mkTransform (dynModularTransform fptr)

foreign import ccall "dynamic"
    dynModularTransform :: FunPtr (Ptr (ℤ/p) -> Ptr (ℤ/p) -> IO ())
                        -> Ptr (ℤ/p)
                        -> Ptr (ℤ/p)
                        -> IO ()

mkTransform :: Storable a
            => (Ptr a -> Ptr a -> IO ())
            -> V.Vector a
            -> V.Vector a
mkTransform f x = unsafePerformIO $ do
    my <- MV.new n
    let (fptr_x, _) = V.unsafeToForeignPtr0 x
    let (fptr_y, _) = MV.unsafeToForeignPtr0 my
    withForeignPtr fptr_x $ \ptr_x ->
      withForeignPtr fptr_y $ \ptr_y ->
        f ptr_x ptr_y
    V.freeze my
  where
    n = V.length x

withCompiledTransform :: (Typed a, Num (Exp a))
                      => Config
                      -> String
                      -> SPL (Exp a)
                      -> (FunPtr b -> IO c)
                      -> IO c
withCompiledTransform conf fname e k = do
    runSpiralWith conf $ do
        t <- toProgram fname e
        c <- C.evalCg $ C.cgProgram t
        when True $ writeOutput dotc (toList c)
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
