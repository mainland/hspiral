{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad (forM_,
                      mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)
import Text.Printf (printf)

import Spiral
import Spiral.Config
import Spiral.Exp
import Spiral.FFT.CooleyTukey
import Spiral.Monad
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Search
import Spiral.Search.FFTBreakdowns
import Spiral.Util.Uniq

data Alg = Alg String (forall m a . MonadConfig m => m a -> m a) (forall a . (Typed a, Typeable a) => SPL (Exp a) -> S () Spiral (SPL (Exp a)))

algs :: [Alg]
algs = [Alg "dif" id radix2DifBreakdown
       ,Alg "dit" id radix2DitBreakdown
       ,Alg "splitradix" id splitRadixSearch
       ,Alg "difrewrite" localDifRewrite radix2DifBreakdown
       ,Alg "improvedsplitradix" id impSplitRadixSearch
       ]

main :: IO ()
main = defaultMain $ \_args -> do
    liftIO $ printf "algorithm,n,muls,add\n"
    forM_ algs $ \(Alg desc lcl f) ->
      forM_ [1..10::Int] $ \n ->
      lcl $ printOpcount desc f (2^n)

localDifRewrite :: MonadConfig m => m a -> m a
localDifRewrite = localConfig (setDynFlag DifRewrite)

printOpcount :: String
             -> (forall a . (Typed a, Typeable a) => SPL (Exp a) -> S () Spiral (SPL (Exp a)))
             -> Int
             -> Spiral ()
printOpcount desc f n = do
  resetUnique
  formula <- runSearch () f (Re (DFT n) :: SPL (Exp Double))
  prog    <- toProgram "f" formula
  ops     <- countProgramOps prog
  liftIO $ printf "%s,%d,%d,%d\n" desc n (mulOps ops) (addOps ops)

radix2DifBreakdown :: Typeable a => SPL (Exp a) -> S () Spiral (SPL (Exp a))
radix2DifBreakdown e@(F 2 _) = return $ spl $ toMatrix e
radix2DifBreakdown (F n w)   = return $ cooleyTukeyDIF 2 (n `div` 2) w
radix2DifBreakdown _         = mzero

radix2DitBreakdown :: Typeable a => SPL (Exp a) -> S () Spiral (SPL (Exp a))
radix2DitBreakdown e@(F 2 _) = return $ spl $ toMatrix e
radix2DitBreakdown (F n w)   = return $ cooleyTukeyDIT 2 (n `div` 2) w
radix2DitBreakdown _         = mzero

splitRadixSearch :: (Typeable a, Typed a, MonadSpiral m)
                 => SPL (Exp a)
                 -> S s m (SPL (Exp a))
splitRadixSearch (F n w) = splitRadixBreakdown n w
splitRadixSearch _       = mzero

impSplitRadixSearch :: (Typeable a, Typed a, MonadSpiral m)
                    => SPL (Exp a)
                    -> S s m (SPL (Exp a))
impSplitRadixSearch (F n w) = improvedSplitRadixBreakdown n w
impSplitRadixSearch _       = mzero
