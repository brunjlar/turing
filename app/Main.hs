{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text      (Text)
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
      let limit = normalizeSteps (maxSteps opts)
      case inputString opts of
        Just input -> runTraceOnce rules limit input
        Nothing    -> runRepl (reloadAction opts) limit rules

printRules :: Rules Char -> IO ()
printRules rules = print rules

reloadAction :: Options -> IO (Either Text (Rules Char))
reloadAction opts = do
  contents <- T.readFile (rulesFile opts)
  pure (parseRules contents)

normalizeSteps :: Maybe Int -> Maybe Int
normalizeSteps maybeLimit = maybeLimit >>= toLimit
  where
    toLimit n
      | n <= 0    = Nothing
      | otherwise = Just n
