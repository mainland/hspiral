{-# LANGUAGE ForeignFunctionInterface #-}

-- Used these as references
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html
-- https://wiki.haskell.org/FFI_complete_examples

module Test.FWHT (
    wht
  ) where

import Foreign
import Foreign.C.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "fwht"
  c_fwht :: Ptr CDouble -> CULong -> IO ()

data Plan = Plan { n :: Int }

plan :: Int -> Plan
plan n = Plan { n = n }

execute :: Plan -> V.Vector Double -> V.Vector Double
execute (Plan n) v = unsafePerformIO $ do
  mv <- V.thaw v
  MV.unsafeWith mv $ \ptr ->
    c_fwht (castPtr ptr) (fromIntegral n)
  V.freeze mv

wht :: Int -> V.Vector Double -> V.Vector Double
wht n = execute (plan n)