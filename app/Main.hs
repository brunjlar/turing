{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO   as T
import           Rewrite        (Rules)
import           Rewrite.Parser (parseRules)
import           Rewrite.Repl   (runRepl)
import           System.Exit    (exitFailure)
import           System.IO      (stderr)
import           Turing.CLI     (Options (..), parseOptions)

main :: IO ()
main = do
  opts <- parseOptions
  processFile (rulesFile opts)

processFile :: FilePath -> IO ()
processFile path = do
  contents <- T.readFile path
  case parseRules contents of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right rules -> do
      printRules rules
      runRepl rules

printRules :: Rules Char -> IO ()
printRules rules = print rules
