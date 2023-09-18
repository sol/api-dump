{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Run (
  main
#ifdef TEST
, Options(..)
, Mode(..)
, Output(..)
, defaultOptions
, parseOptions

, Module(..)
, parseModule
, dumpModuleApi
#endif
) where

import Prelude hiding (writeFile)

import Data.Char
use Format

import Data.Set (Set)
use Data.Set

data Module = Module [String]
  deriving (Eq, Show)

instance ToString Module where
  toString (Module components) = components.join "."

instance HasField "toString" Module String where
  getField = toString

parseModule :: String -> Maybe Module
parseModule arg = case String.split "." arg of
  xs | isModule xs -> Just (Module xs)
  _ -> Nothing

isModule :: [String] -> Bool
isModule = \ case
  [] -> True
  (unpack -> c : _) : xs -> isUpper c && isModule xs
  _ -> False

partitionMaybe :: forall a b. (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe p = go
  where
    go [] = ([], [])
    go (a : as) = case p a of
      Nothing -> first (a :) $ go as
      Just b -> second (b :) $ go as

data Mode = RawMode | PrettyMode
  deriving (Eq, Show)

data Output = StdOut | FileSystem
  deriving (Eq, Show)

data Options = Options {
  modules :: [Module]
, mode :: Mode
, output :: Output
} deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options [] PrettyMode FileSystem

parseOptions :: [String] -> IO Options
parseOptions args = case partitionMaybe parseModule . concatMap words $ args of
  (parseFlags -> (options, []), modules) -> return options { modules }
  _ -> Process.exit "usage: {} [--raw] [--stdout]"
  where
    parseFlags :: [String] -> (Options, [String])
    parseFlags = fmap Set.toList . uncurry parseOutput . parseMode defaultOptions . Set.fromList

    parseMode :: Options -> Set String -> (Options, Set String)
    parseMode options flags = case hasFlag "--raw" flags of
      Just flags_ -> (options { mode = RawMode } , flags_)
      Nothing -> (options, flags)

    parseOutput :: Options -> Set String -> (Options, Set String)
    parseOutput options flags = case hasFlag "--stdout" flags of
      Just flags_ -> (options { output = StdOut } , flags_)
      Nothing -> (options, flags)

    hasFlag :: String -> Set String -> Maybe (Set String)
    hasFlag flag flags
      | Set.member flag flags = Just (Set.delete flag flags)
      | otherwise = Nothing

main :: IO ()
main = Process.args >>= parseOptions >>= \ case
  options -> run (format options) (output options) options.modules
  where
    format options = case options.mode of
      RawMode -> id
      PrettyMode -> Format.format

    output :: Options -> Module -> String -> IO ()
    output options module_ = case options.output of
      FileSystem -> writeFile ("api" </> module_.toString.asFilePath)
      StdOut -> stdout.write

run :: (String -> String) -> (Module -> String -> IO ()) -> [Module] -> IO ()
run format output modules = do
  forM_ modules $ \ module_ -> do
    dumpModuleApi format module_ >>= output module_

writeFile :: FilePath -> String -> IO ()
writeFile name contents = do
  Directory.ensure name.directory
  IO.writeFile name contents

dumpModuleApi :: (String -> String) -> Module -> IO String
dumpModuleApi format module_ = browse module_ >>= removePackageVersions . format

browse :: Module -> IO String
browse module_ = ByteString.decodeUtf8 <$>
  Process.command("ghci", ["-v0", "-ignore-dot-ghci"]).stdin.set(":m { module_}\n:browse {module_}").read

removePackageVersions :: String -> IO String
removePackageVersions = fmap ByteString.decodeUtf8 . Process.read . Process.command("sed", ["s/-[[:digit:]]\\(\\.[[:digit:]]\\+\\)\\+:/:/g"]).stdin.set
