{-# LANGUAGE OverloadedStrings #-}

module Turing.CLI
  ( Options (..)
  , optionsParser
  , parserInfo
  , parseOptions
  ) where

import           Options.Applicative
import           Text.Read           (readMaybe)

data Options = Options
  { rulesFile   :: FilePath
  , inputString :: Maybe String
  , maxSteps    :: Maybe Int
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
        ( metavar "RULES-FILE"
       <> help "Path to the rewrite rules file"
        )
  <*> optional (strOption
        ( long "input"
       <> metavar "STRING"
       <> help "Run in non-interactive mode by tracing STRING and exiting"
        ))
  <*> optional (option nonNegative
        ( long "max-steps"
       <> metavar "N"
       <> help "Maximum number of rewrite steps to display (requires --input)"
        ))
  where
    nonNegative :: ReadM Int
    nonNegative = maybeReader $ \s -> do
      n <- readMaybe s
      if n >= 0 then Just n else Nothing

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  ( fullDesc
 <> progDesc "Rewrite input strings using rules from a file."
 <> header "turing - a string rewriting playground"
  )

parseOptions :: IO Options
parseOptions = execParser parserInfo
