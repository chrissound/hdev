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
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.Map as M
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Options.Applicative

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
  forM_ (zip [1..] exes) $ \(i, exe) -> 
    putStrLn $ show i <> ") " <> T.unpack exe
  selection <- getLine
  case reads selection of
    [(n, "")] | n > 0 && n <= length exes -> return $ exes !! (n-1)
    _ -> do
      putStrLn "Invalid selection. Please try again."
      interactiveSelect exes

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
      
  let p = case envFile of
        Just env -> proc "bash" ["-c", mconcat ["source ", env, " && exec ghcid ", unwords args]]
        Nothing -> proc "ghcid" args
      processConfig = setDelegateCtlc True p
  print p
  _ <- runProcess processConfig
  return ()

runExecWithCmd :: T.Text -> Maybe String -> [String] -> IO ()
runExecWithCmd exe envFile extraArgs = do
  let buildCmd env = mconcat $ 
        maybe [] (\e -> ["source ", e, " && "]) env ++
        ["cabal run ", T.unpack exe, " -- ", unwords extraArgs]
      cmd = buildCmd envFile
      p = proc "bash" ["-c", cmd]
      processConfig = setCreateGroup True p
  print p
  runProcess_ processConfig

data CmdOptions = CmdOptions
  { optEnvFile :: Maybe String
  , optList :: Bool
  , optRun :: Bool
  , optLint :: Bool
  , optExecWithCmd :: Bool
  , optExecutable :: Maybe String
  , optExtraArgs :: [String]
  } deriving (Show)

optionsParser :: Parser CmdOptions
optionsParser = CmdOptions
  <$> optional (strOption (long "env" <> metavar "FILE" <> help "Environment file to source"))
  <*> switch (long "list" <> help "List all executables")
  <*> switch (long "run" <> help "Run with ghcid")
  <*> switch (long "lint" <> help "Run with ghcid in lint mode")
  <*> switch (long "exec-with-cmd" <> help "Execute with cabal run")
  <*> optional (argument str (metavar "EXECUTABLE"))
  <*> many (argument str (metavar "ARGS..."))

opts :: ParserInfo CmdOptions
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Haskell development tool"
  <> header "hdev - Usage: script [--env <file>] [--list | --run [<executable>] | --lint [<executable>] | --exec-with-cmd [<executable>] [args...]]" )

main :: IO ()
main = do
  options <- execParser opts
  decodeResult <- parsePackageYaml
  let exes = getExecutables decodeResult
      selectAndRun lint env = interactiveSelect exes >>= \exe -> runGhcid exe lint env decodeResult
      selectAndExec env extraArgs = interactiveSelect exes >>= \exe -> runExecWithCmd exe env extraArgs
      env = optEnvFile options
      exe = fmap T.pack (optExecutable options)
      
  case () of
    _ | optList options -> mapM_ TIO.putStrLn exes
      | optRun options -> maybe (selectAndRun False env) (\e -> runGhcid e False env decodeResult) exe
      | optLint options -> maybe (selectAndRun True env) (\e -> runGhcid e True env decodeResult) exe
      | optExecWithCmd options -> maybe (selectAndExec env (optExtraArgs options)) (\e -> runExecWithCmd e env (optExtraArgs options)) exe
      | otherwise -> selectAndRun False env  -- Default behavior
