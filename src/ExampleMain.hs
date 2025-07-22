module ExampleMain where

import System.Environment

main = do
  args <- getArgs
  case args of
    ("--example":"--abc123":[]) -> print "Example abc123"
    _ -> error $ "Invalid params:" ++ show args
