#!/usr/bin/env cabal
{- cabal:
build-depends: base, hpack, typed-process, directory, filepath, text
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hpack.Config
import Hpack
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process.Typed
import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.Map as M
import Data.List (isSuffixOf)

parsePackageYaml :: IO DecodeResult
parsePackageYaml = do
  result <- readPackageConfig defaultDecodeOptions
  case result of
    Right decodeResult -> return decodeResult
    Left err -> error $ "Failed to parse package.yaml: " ++ err

getExecutables :: DecodeResult -> [T.Text]
getExecutables decodeResult = 
  map T.pack $ M.keys $ packageExecutables $ decodeResultPackage decodeResult

getExecutableMain :: DecodeResult -> T.Text -> T.Text
getExecutableMain decodeResult exeName = 
  case M.lookup (T.unpack exeName) (packageExecutables $ decodeResultPackage decodeResult) of
    Just section -> case executableMain (sectionData section) of
      Just mainFile -> T.pack mainFile
      Nothing -> error $ "No main file found for executable: " ++ T.unpack exeName
    Nothing -> error $ "Executable not found: " ++ T.unpack exeName

interactiveSelect :: [T.Text] -> IO T.Text
interactiveSelect exes = do
  putStrLn "Select an executable:"
  mapM_ (\(i, exe) -> putStrLn $ show i ++ ") " ++ T.unpack exe) (zip [1..] exes)
  choice <- getLine
  case reads choice of
    [(n, "")] | n > 0 && n <= length exes -> return $ exes !! (n-1)
    _ -> interactiveSelect exes

runGhcid :: T.Text -> Bool -> Maybe String -> DecodeResult -> IO ()
runGhcid exe lintMode envFile decodeResult = do
  cwd <- getCurrentDirectory
  let v = takeFileName cwd
      mainFile = getExecutableMain decodeResult exe
      mainModule = T.dropEnd 3 mainFile
      
  cabalFiles <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
  when (null cabalFiles) $ runProcess_ (proc "hpack" [])
  unless (null cabalFiles) $ do
    packageYamlNewer <- (>) <$> getModificationTime "package.yaml" <*> getModificationTime (head cabalFiles)
    when packageYamlNewer $ runProcess_ (proc "hpack" [])
  
  let ghcidArgs = ["--command", "cabal v2-repl exe:" ++ T.unpack exe, "-o", "/tmp2/" ++ v ++ ".ghcid"]
      testArgs = if lintMode then [] else ["--test", T.unpack mainModule ++ ".main"]
      envSource = maybe "env.sh" id envFile
      
  let p = proc "bash" ["-c", "source " ++ envSource ++ " && ghcid " ++ unwords (ghcidArgs ++ testArgs)]
  runProcess_ $ p

main :: IO ()
main = do
  args <- getArgs
  decodeResult <- parsePackageYaml
  let exes = getExecutables decodeResult
      
  case args of
    ["--list"]                        -> mapM_ TIO.putStrLn exes
    ["--run"]                         -> interactiveSelect exes >>= \exe -> runGhcid exe False Nothing decodeResult
    ["--run", exe]                    -> runGhcid (T.pack exe) False Nothing decodeResult
    ["--lint"]                        -> interactiveSelect exes >>= \exe -> runGhcid exe True Nothing decodeResult
    ["--lint", exe]                   -> runGhcid (T.pack exe) True Nothing decodeResult
    ["--env", envFile, "--run"]       -> interactiveSelect exes >>= \exe -> runGhcid exe False (Just envFile) decodeResult
    ["--env", envFile, "--run", exe]  -> runGhcid (T.pack exe) False (Just envFile) decodeResult
    ["--env", envFile, "--lint"]      -> interactiveSelect exes >>= \exe -> runGhcid exe True (Just envFile) decodeResult
    ["--env", envFile, "--lint", exe] -> runGhcid (T.pack exe) True (Just envFile) decodeResult
    []                                -> interactiveSelect exes >>= \exe -> runGhcid exe False Nothing decodeResult
    _                                 -> putStrLn "Usage: script [--env <file>] [--list | --run [<executable>] | --lint [<executable>]]" >> exitFailure
