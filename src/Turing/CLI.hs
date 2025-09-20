{-# LANGUAGE OverloadedStrings #-}

module Turing.CLI
  ( Options (..)
  , optionsParser
  , parserInfo
  , parseOptions
  ) where

import           Options.Applicative

newtype Options = Options
  { rulesFile :: FilePath
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options <$> argument str
  ( metavar "RULES-FILE"
 <> help "Path to the rewrite rules file"
  )

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  ( fullDesc
 <> progDesc "Rewrite input strings using rules from a file."
 <> header "turing - a string rewriting playground"
  )

parseOptions :: IO Options
parseOptions = execParser parserInfo
