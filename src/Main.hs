#!/usr/bin/env cabal
{- cabal:
build-depends: base, hpack, typed-process, directory, filepath, text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Maybe (fromMaybe)

parsePackageYaml :: IO DecodeResult
parsePackageYaml = readPackageConfig defaultDecodeOptions >>= \case
  Right decodeResult -> return decodeResult
  Left err -> error $ "Failed to parse package.yaml: " ++ err

getExecutables :: DecodeResult -> [T.Text]
getExecutables decodeResult = 
  map T.pack $ M.keys $ packageExecutables $ decodeResultPackage decodeResult

getExecutableMain :: DecodeResult -> T.Text -> T.Text
getExecutableMain decodeResult exeName = 
  case M.lookup (T.unpack exeName) (packageExecutables $ decodeResultPackage decodeResult) of
    Just section -> maybe (error $ "No main file found for executable: " ++ T.unpack exeName)
                          T.pack
                          (executableMain $ sectionData section)
    Nothing -> error $ "Executable not found: " ++ T.unpack exeName

interactiveSelect :: [T.Text] -> IO T.Text
interactiveSelect exes = do
  putStrLn "Select an executable:"
  mapM_ (\(i, exe) -> putStrLn $ show i ++ ") " ++ T.unpack exe) (zip [1..] exes)
  getLine >>= \choice -> case reads choice of
    [(n, "")] | n > 0 && n <= length exes -> return $ exes !! (n-1)
    _ -> interactiveSelect exes

runGhcid :: T.Text -> Bool -> Maybe String -> DecodeResult -> IO ()
runGhcid exe lintMode envFile decodeResult = do
  cwd <- getCurrentDirectory
  let v = takeFileName cwd
      ghcidOutput = "/tmp2/" ++ v ++ ".ghcid"
      mainFile = getExecutableMain decodeResult exe
      mainModule = T.dropEnd 3 mainFile
      
  cabalFiles <- filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."
  if (null cabalFiles)
     then runProcess_ (proc "hpack" [])
     else do
        packageYamlNewer <- (>) <$> getModificationTime "package.yaml" <*> getModificationTime (head cabalFiles)
        when packageYamlNewer $ runProcess_ (proc "hpack" [])
  
  let ghcidArgs = ["--command", mconcat ["cabal v2-repl exe:", T.unpack exe], "-o", ghcidOutput]
      testArgs = if lintMode then [] else ["--test", T.unpack mainModule ++ ".main"]
      args = ghcidArgs ++ testArgs
      envSource = fromMaybe "env.sh" envFile
      
  let p = proc "bash" [ "-c", mconcat ["source ", envSource, " &&",  "ghcid " ++ unwords args]]
  print p
  runProcess_ p

runExecWithCmd :: T.Text -> Maybe String -> [String] -> IO ()
runExecWithCmd exe envFile extraArgs = do
  let cmd = case envFile of
        Just env -> mconcat ["source ", env," && cabal run ", T.unpack exe, " -- ", unwords extraArgs]
        Nothing -> "cabal run " ++ T.unpack exe ++ " -- " ++ unwords extraArgs
  let p = proc "bash" ["-c", cmd]
  print p
  runProcess_ p

main :: IO ()
main = do
  args <- getArgs
  decodeResult <- parsePackageYaml
  let exes = getExecutables decodeResult
      selectAndRun lint env = interactiveSelect exes >>= \exe -> runGhcid exe lint env decodeResult
      selectAndExec env extraArgs = interactiveSelect exes >>= \exe -> runExecWithCmd exe env extraArgs
      
  case args of
    ["--list"]                        -> mapM_ TIO.putStrLn exes
    ["--run"]                         -> selectAndRun False Nothing
    ["--run", exe]                    -> runGhcid (T.pack exe) False Nothing decodeResult
    ["--lint"]                        -> selectAndRun True Nothing
    ["--lint", exe]                   -> runGhcid (T.pack exe) True Nothing decodeResult
    ["--env", envFile, "--run"]       -> selectAndRun False (Just envFile)
    ["--env", envFile, "--run", exe]  -> runGhcid (T.pack exe) False (Just envFile) decodeResult
    ["--env", envFile, "--lint"]      -> selectAndRun True (Just envFile)
    ["--env", envFile, "--lint", exe] -> runGhcid (T.pack exe) True (Just envFile) decodeResult
    ("--exec-with-cmd":exe:rest)      -> runExecWithCmd (T.pack exe) Nothing rest
    ("--exec-with-cmd":rest)          -> selectAndExec Nothing rest
    ("--env":envFile:"--exec-with-cmd":exe:rest) -> runExecWithCmd (T.pack exe) (Just envFile) rest
    ("--env":envFile:"--exec-with-cmd":rest)      -> selectAndExec (Just envFile) rest
    []                                -> selectAndRun False Nothing
    _                                 -> putStrLn "Usage: script [--env <file>] [--list | --run [<executable>] | --lint [<executable>] | --exec-with-cmd [<executable>] [args...]]" >> exitFailure
