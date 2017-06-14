{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Spiral.Driver.Opts
-- Copyright   :  (c) 2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Spiral.Driver.Opts (
    parseOpts,
    parseOpts',
    usage,
    usage'
  ) where

import Control.Monad ((>=>))
import System.Console.GetOpt
import System.Environment (getProgName)

import Spiral.Config
import Spiral.Globals

options :: forall m . Monad m => [OptDescr (Config -> m Config)]
options =
    [ Option ['h', '?'] ["help"]    (NoArg (setModeM Help))              "Show help"
    , Option ['q']      ["quiet"]   (NoArg (setDynFlagM Quiet))          "Be quiet"
    , Option ['v']      ["verbose"] (OptArg maybeSetVerbLevel "LEVEL")   "Be verbose"
    , Option ['c']      []          (NoArg (setModeM Compile))           "Compile"
    , Option ['o']      ["output"]  (ReqArg outOpt "FILE")               "Output to FILE"
    , Option ['f']      []          (ReqArg parseFFlags "")              "Specify compiler options"
    , Option ['d']      []          (ReqArg parseDFlags "")              "Specify debug options"
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
fFlags = [ (LinePragmas,       "line-pragmas",       "Print line pragmas in generated C")
         , (UseComplex,        "use-complex",        "Use C99 _Complex type")
         , (ThreeMults,        "three-mult",         "Use real three-multiplication variant of complex multiply")
         , (StoreIntermediate, "store-intermediate", "Explicitly store intermediate results")
         , (SplitComplex,      "split-complex",      "Always split complex numbers when performing CSE them")
         , (CSE,               "cse",                "Perform common subexpression elimination")
         , (Rewrite,           "rewrite",            "Apply rewrite rules")
         ]

fOpts :: forall m . Monad m => [FlagOptDescr (Config -> m Config)]
fOpts =
    [FlagOption "max-unroll" (ReqArg maxUnroll "INT") "Set maximum number of iterations to automatically unroll"]
  where
    maxUnroll :: String -> Config -> m Config
    maxUnroll s fs =
      case reads s of
        [(n, "")]  -> return fs { maxUnroll = n }
        _          -> fail "argument to -fmax-unroll must be an integer"

dFlags :: [(DynFlag, String, String)]
dFlags = [(GenComments, "gen-comments", "Add comments in generated code")]

dTraceFlags :: [(TraceFlag, String, String)]
dTraceFlags = [(TraceCg,     "cg",     "Trace code generation")
              ,(TraceCache,  "cache",  "Trace caching")
              ,(TraceSearch, "search", "Trace search")]

dOpts :: [FlagOptDescr (Config -> m Config)]
dOpts = []

parseOpts :: [String] -> IO (Config, [String])
parseOpts argv = do
    (config, _, n) <- parseOpts' argv []
    return (config, n)

parseOpts' :: forall o . [String] -> [OptDescr o] -> IO (Config, [o], [String])
parseOpts' argv opts =
    case getOpt Permute (mergeOpts opts) argv of
      (fs,n,[])  -> do let fs_left  = [x | Left x <- fs]
                           fs_right = [x | Right x <- fs]
                       config <- optsToConfig fs_left
                       return (config, fs_right, n)
      (_,_,errs) -> do usageDesc <- usage' opts
                       ioError (userError (concat errs ++ usageDesc))

mergeOpts :: Monad m => [OptDescr o] -> [OptDescr (Either (Config -> m Config) o)]
mergeOpts opts = map (fmap Left) options ++ map (fmap Right) opts

optsToConfig :: [Config -> IO Config] -> IO Config
optsToConfig fs = do
    config <- foldl (>=>) return fs defaultConfig
    setThreeMults $ testDynFlag ThreeMults config
    return config

usage :: IO String
usage = usage' []

usage' :: forall o . [OptDescr o] -> IO String
usage' opts = do
    progname   <- getProgName
    let header =  "Usage: " ++ progname ++ " [OPTION...] files..."
    return $ usageInfo header allOpts ++ "\n" ++
             "  Compiler options:\n" ++
             unlines (justify $ map (flagOpt2Desc "-f") fOpts ++
                                map (flag2Desc "-f")    fFlags) ++
             "\n" ++
             "  Debug options:\n" ++
             unlines (justify $ map (flag2Desc "-d")       dFlags ++
                                map (flag2Desc "-dtrace-") dTraceFlags)
  where
    allOpts :: [OptDescr (Either (Config -> IO Config) o)]
    allOpts = mergeOpts opts

    flagOpt2Desc :: String -> FlagOptDescr (Config -> IO Config) -> (String, String)
    flagOpt2Desc pfx (FlagOption opt arg desc) = (pfx ++ opt ++ arg2Desc arg, desc)
      where
        arg2Desc :: ArgDescr a -> String
        arg2Desc NoArg{}         = ""
        arg2Desc (ReqArg _ desc) = "=" ++ desc
        arg2Desc (OptArg _ desc) = "[]=" ++ desc ++ "]"

    flag2Desc :: String -> (a, String, String) -> (String, String)
    flag2Desc pfx (_, opt, desc) = (pfx ++ opt, desc)

    justify :: [(String, String)] -> [String]
    justify flags = ["    " ++ justify1 flag ++ "  " ++ desc | (flag, desc) <- flags]
      where
        n = maximum [length flag | (flag, _) <- flags]

        justify1 :: String -> String
        justify1 s = s ++ replicate (n - length s) ' '
