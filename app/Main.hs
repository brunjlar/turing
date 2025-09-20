{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO   as T
import           Rewrite        (Rules)
import           Rewrite.Parser (parseRules)
import           Rewrite.Repl   (runRepl, runTraceOnce)
import           System.Exit    (exitFailure)
import           System.IO      (stderr)
import           Turing.CLI     (Options (..), parseOptions)

main :: IO ()
main = do
  opts <- parseOptions
  processFile opts

processFile :: Options -> IO ()
processFile opts = do
  contents <- T.readFile (rulesFile opts)
  case parseRules contents of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right rules -> do
      printRules rules
      case inputString opts of
        Just input -> runTraceOnce rules (maxSteps opts) input
        Nothing    -> runRepl rules

printRules :: Rules Char -> IO ()
printRules rules = print rules
