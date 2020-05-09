{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import Data.Foldable (toList)
import Data.List (nub,
                  sort)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class
import Text.Read (readMaybe)

import Spiral
import Spiral.Backend.C
import Spiral.Config
import Spiral.Driver
import Spiral.Exp
import Spiral.Search.OpCount
import Spiral.OpCount
import Spiral.SPL
import Spiral.SPL.Run
import Spiral.Util.Uniq

parseSizesArg :: Monad m => String -> m [Int]
parseSizesArg = fmap concat . mapM parseRange . splitOn (== ',')

parseRange :: Monad m => String -> m [Int]
parseRange s =
    case splitOn (== '-') s of
      [x]   -> do start <- parseInt x
                  return [start]
      [x,y] -> do start <- parseInt x
                  end   <- parseInt y
                  return [start..end]
      _     -> fail $ "Cannot parse range: " ++ s

parseInt :: Monad m => String -> m Int
parseInt s = case readMaybe s of
                 Nothing -> fail $ "Cannot parse integer: " ++ s
                 Just n  -> return n

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                  "" -> []
                  s' -> w : splitOn p s''
                        where (w, s'') = break p s'

main :: IO ()
main =
    defaultMainWith mempty $ \args -> do
    sizes <- nub . sort . concat <$> mapM parseSizesArg args
    useComplexType <- asksConfig $ testDynFlag UseComplex
    liftIO $ putDocLn $ text "\"size\",\"totalops\",\"mulops\",\"addops\""
    mapM_ (search useComplexType) sizes
  where
    search :: Bool -> Int -> Spiral ()
    search True  n = searchOpCount (DFT n :: SPL (Exp (Complex Double))) >>= go n
    search False n = searchOpCount (Re (DFT n) :: SPL (Exp Double)) >>= go n

    go :: (Typed a, Num (Exp a)) => Int -> SPL (Exp a) -> Spiral ()
    go n e = do
        -- pprint e
        prog <- toProgram ("hspiral_dft_" ++ show n) e
        -- pprint prog
        ops <- countProgramOps prog
        resetUnique
        defs <- evalCg $ cgProgram prog
        outp <- asksConfig output
        case outp of
          Nothing -> return ()
          Just{}  -> writeOutput (toList defs)
        liftIO $ putDocLn $ folddoc (\x y -> x <> comma <> y) [ppr n, ppr (allOps ops), ppr (mulOps ops), ppr (addOps ops)]
