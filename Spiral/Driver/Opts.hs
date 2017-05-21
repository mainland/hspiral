{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Driver.Opts
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Driver.Opts (
    parseOpts,
    usage
  ) where

import Control.Monad ((>=>))
import System.Console.GetOpt
import System.Environment (getProgName)

import Spiral.Config

options :: forall m . Monad m => [OptDescr (Config -> m Config)]
options =
    [ Option ['h', '?'] ["--help"]  (NoArg (setModeM Help))              "Show help"
    , Option ['q']      ["quiet"]   (NoArg (setDynFlagM Quiet))          "Be quiet"
    , Option ['v']      ["verbose"] (OptArg maybeSetVerbLevel "LEVEL")   "Be verbose"
    , Option ['c']      []          (NoArg (setModeM Compile))           "Compile"
    , Option ['o']      ["output"]  (ReqArg outOpt "FILE")               "Output to FILE"
    , Option ['f']      []          (ReqArg parseFFlags "")              "Specify compiler options"
    , Option ['d']      []          (ReqArg parseDFlags "")              "Specify debug flags"
    ]
  where
    setModeM :: ModeFlag -> Config -> m Config
    setModeM m fs = return fs { mode = m }

    setDynFlagM :: DynFlag -> Config -> m Config
    setDynFlagM f fs = return $ setDynFlag f fs

    maybeSetVerbLevel :: Maybe String -> Config -> m Config
    maybeSetVerbLevel Nothing fs =
        return fs { verbLevel = verbLevel fs + 1 }

    maybeSetVerbLevel (Just s) fs =
        case reads s of
          [(n, "")]  -> return fs { verbLevel = n }
          _          -> fail "argument to --verbose must be an integer"

    outOpt :: String -> Config -> m Config
    outOpt path fs = return fs { output = Just path }

    parseFFlags :: String -> Config -> m Config
    parseFFlags = parseFlagOpts "-f" opts fOpts
      where
        opts :: [FlagOpt]
        opts = mkFlagOpts "" fFlags setDynFlag (Just unsetDynFlag)

    parseDFlags :: String -> Config -> m Config
    parseDFlags = parseFlagOpts "-d" opts dOpts
      where
        opts :: [FlagOpt]
        opts =
            mkFlagOpts ""       dFlags      setDynFlag   (Just unsetDynFlag) ++
            mkFlagOpts "trace-" dTraceFlags setTraceFlag Nothing

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn x s = case break (== x) s of
              (xs, []) -> (xs, [])
              (xs, ys) -> (xs, drop 1 ys)

data FlagOpt = forall a . FlagOpt a String String (a -> Config -> Config) (Maybe (a -> Config -> Config))

data FlagOptDescr a = FlagOption String (ArgDescr a) String

parseFlagOpts :: forall m . Monad m
              => String
              -> [FlagOpt]
              -> [FlagOptDescr (Config -> m Config)]
              -> String
              -> Config
              -> m Config
parseFlagOpts flagpfx fopts foptdescrs arg fs =
    if null flagArg
      then do maybe_fs' <- parseFlag flag fopts fs
              case maybe_fs' of
                Nothing  -> parseOpts flag flagArg foptdescrs fs
                Just fs' -> return fs'
      else parseOpts flag flagArg foptdescrs fs
  where
    flag, flagArg :: String
    (flag, flagArg) = splitOn '=' arg

    parseOpts :: String
              -> String
              -> [FlagOptDescr (Config -> m Config)]
              -> Config
              -> m Config
    parseOpts _ _ [] = fail $ "unrecognized option `" ++ flagpfx ++ arg ++ "'"

    parseOpts flag flagArg (FlagOption flag' argOpt _:_) | flag' == flag =
        go argOpt
      where
        go :: ArgDescr (Config -> m Config) -> Config -> m Config
        go (NoArg g)    | null flagArg = g
                        | otherwise    = fail $ "Argument specified:" ++ arg
        go (OptArg g _) | null flagArg = g Nothing
                        | otherwise    = g (Just flagArg)
        go (ReqArg g _) | null flagArg = fail $ "Argument required:" ++ arg
                        | otherwise    = g flagArg

    parseOpts flag flagArg (_:opts) =
        parseOpts flag flagArg opts

parseFlag :: forall m . Monad m
          => String
          -> [FlagOpt]
          -> Config
          -> m (Maybe Config)
parseFlag flag = go
  where
    go :: [FlagOpt] -> Config -> m (Maybe Config)
    go [] =
        return . const Nothing

    go (FlagOpt f s _ set Nothing:fs')
      | flag == s = return . Just . set f
      | otherwise = go fs'

    go (FlagOpt f s _ set (Just unset):fs')
      | flag == s          = return . Just . set f
      | flag == "no" ++ s  = return . Just . unset f
      | flag == "no-" ++ s = return . Just . unset f
      | otherwise          = go fs'

mkFlagOpts :: String
           -> [(a, String, String)]
           -> (a -> Config -> Config)
           -> Maybe (a -> Config -> Config)
           -> [FlagOpt]
mkFlagOpts pfx opts set unset =
    [FlagOpt f (pfx ++ s) desc set unset | (f, s, desc) <- opts]

fFlags :: [(DynFlag, String, String)]
fFlags = [ (LinePragmas, "line-pragmas", "print line pragmas in generated C")
         , (UseComplex,  "use-complex",  "use C99 _Complex type")
         ]

fOpts :: forall m . Monad m => [FlagOptDescr (Config -> m Config)]
fOpts =
    [FlagOption "max-unroll" (ReqArg maxUnroll "INT") "set maximum number of iterations to automatically unroll"]
  where
    maxUnroll :: String -> Config -> m Config
    maxUnroll s fs =
      case reads s of
        [(n, "")]  -> return fs { maxUnroll = n }
        _          -> fail "argument to -fmax-unroll must be an integer"

dFlags :: [(DynFlag, String, String)]
dFlags = [(GenComments, "gen-comments", "add comments in generated code")]

dTraceFlags :: [(TraceFlag, String, String)]
dTraceFlags = [(TraceCg,     "cg",     "trace code generation")
              ,(TraceSearch, "search", "trace DFT search")]

dOpts :: [FlagOptDescr (Config -> m Config)]
dOpts = []

parseOpts :: [String] -> IO (Config, [String])
parseOpts argv =
    case getOpt Permute options argv of
      (fs,n,[])  -> do config <- foldl (>=>) return fs defaultConfig
                       return (config, n)
      (_,_,errs) -> do usageDesc <- usage
                       ioError (userError (concat errs ++ usageDesc))

usage :: IO String
usage = do
    progname   <- getProgName
    let header =  "Usage: " ++ progname ++ " [OPTION...] files..."
    return $ usageInfo header (options :: [OptDescr (Config -> IO Config)])
