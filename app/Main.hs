{-# LANGUAGE OverloadedStrings #-}

-- | Entry point for the rewrite REPL executable.
module Main (main) where

import           Data.Text      (Text)
import qualified Data.Text.IO   as T
import           Rewrite        (Rules)
import           Rewrite.Parser (parseRules)
import           Rewrite.Repl   (runRepl, runTraceOnce)
import           System.Exit    (exitFailure)
import           System.IO      (stderr)
import           Turing.CLI     (Options (..), parseOptions)

-- | Parse CLI options and dispatch to the REPL or one-off trace.
main :: IO ()
main = do
  opts <- parseOptions
  processFile opts

-- | Load rules from disk and either run the REPL or trace a single input.
processFile :: Options -> IO ()
processFile opts = do
  contents <- T.readFile (rulesFile opts)
  case parseRules contents of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right rules -> do
      printRules rules
      let limit = normalizeSteps (maxSteps opts)
      case inputString opts of
        Just input -> runTraceOnce rules limit input
        Nothing    -> runRepl (reloadAction opts) limit rules

-- | Echo the parsed rules so users can verify what will run.
printRules :: Rules Char -> IO ()
printRules rules = print rules

-- | Reload the rules file for the REPL, returning parse errors verbatim.
reloadAction :: Options -> IO (Either Text (Rules Char))
reloadAction opts = do
  contents <- T.readFile (rulesFile opts)
  pure (parseRules contents)

-- | Convert the optional CLI limit into the internal representation.
normalizeSteps :: Maybe Int -> Maybe Int
normalizeSteps maybeLimit = maybeLimit >>= toLimit
  where
    toLimit n
      | n <= 0    = Nothing
      | otherwise = Just n
