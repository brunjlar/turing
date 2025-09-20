{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO       as T
import           Rewrite            (Rules)
import           Rewrite.Parser     (parseRules)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> processFile path
    _      -> do
      hPutStrLn stderr "Usage: turing <rules-file>"
      exitFailure

processFile :: FilePath -> IO ()
processFile path = do
  contents <- T.readFile path
  case parseRules contents of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right rules -> printRules rules

printRules :: Rules Char -> IO ()
printRules rules = print rules
