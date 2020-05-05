{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
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

data SearchConfig = SearchConfig
    { -- | Start of search range
      rangeStart :: Maybe Int

    , -- | End of search range
      rangeEnd :: Maybe Int
    }
  deriving (Eq, Ord, Show)

instance Semigroup SearchConfig where
    x <> y = SearchConfig
        { rangeStart = rangeStart x `rightBiased` rangeStart y
        , rangeEnd   = rangeEnd x `rightBiased` rangeEnd y
      }
      where
        rightBiased :: Maybe a -> Maybe a -> Maybe a
        rightBiased Nothing Nothing = Nothing
        rightBiased (Just x) Nothing = Just x
        rightBiased _ (Just x) = Just x

instance Monoid SearchConfig where
    mempty = SearchConfig
        { rangeStart = Nothing
        , rangeEnd   = Nothing
        }

    mappend = (<>)

searchOpts :: forall m . Monad m => [OptDescr (SearchConfig -> m SearchConfig)]
searchOpts =
    [ Option [] ["start"] (ReqArg start "INT") "Set start of range"
    , Option [] ["end"]   (ReqArg end "INT")   "Set end of range"
    ]
  where
    start :: String -> SearchConfig -> m SearchConfig
    start s config =
        case readMaybe s of
            Nothing -> fail $ "Illegal start range " ++ s
            Just n  -> return config { rangeStart = Just n }

    end :: String -> SearchConfig -> m SearchConfig
    end s config =
        case readMaybe s of
            Nothing -> fail $ "Illegal end range " ++ s
            Just n  -> return config { rangeEnd = Just n }

main :: IO ()
main =
    defaultMainWith' searchOpts mempty $ \opts _args -> do
    config <- combineOpts opts
    useComplexType <- asksConfig $ testDynFlag UseComplex
    liftIO $ putDocLn $ text "\"size\",\"totalops\",\"mulops\",\"addops\""
    mapM_ (search useComplexType) [fromMaybe 2 (rangeStart config)..fromMaybe 64 (rangeEnd config)]
  where
    combineOpts :: (Monoid a, Monad m) => [a -> m a] -> m a
    combineOpts opts = foldl (>=>) return opts mempty

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
