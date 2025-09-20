{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Hspec       (Spec, describe, it, runIO, shouldBe,
                                   shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (testSpec)

import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO

import           Rewrite          (Rule (..), Rules, trace)
import           Rewrite.Parser   (parseRules)
import           Rewrite.Repl     (renderTraceLines, renderTraceLinesLimited)
import           Turing.CLI       (Options (..), parserInfo)

import           Options.Applicative (ParserResult (..), defaultPrefs,
                                      execParserPure)
import           Test.QuickCheck (Property, (===), chooseInt, forAll)

main :: IO ()
main = do
  hspecTests <- testSpec "parser" unitSpecs
  defaultMain $ testGroup "turing" [hspecTests]

unitSpecs :: Spec
unitSpecs = do
  describe "parseRules" $ do
    it "parses a single rule" $ do
      let input = "abc -> ABC;"
      fmap rulesToTuples (parseRules input) `shouldBe`
        Right [("abc", "ABC")]

    it "parses rules with whitespace and comments" $ do
      let input :: Text
          input = T.unlines
            [ "-- simple duplication rule"
            , "  ->   ;"
            , ""
            , "{- uppercase -}"
            , "abc->ABC;"
            ]
      fmap rulesToTuples (parseRules input) `shouldBe`
        Right [("  ", "   "), ("abc", "ABC")]

    it "parses whitespace-only rule via escapes" $ do
      let input = "\\s -> \\s\\s;"
      fmap rulesToTuples (parseRules input) `shouldBe`
        Right [(" ", "  ")]

    it "parses empty lhs" $ do
      fmap rulesToTuples (parseRules "->X;") `shouldBe`
        Right [("", "X")]

    it "allows comments between tokens" $ do
      let input = T.unlines
            ["ab{- ignore this -} -> {- rhs -}CD;" ,"x->y;"]
      fmap rulesToTuples (parseRules input) `shouldBe`
        Right [("ab", "CD"), ("x", "y")]

    it "parses literal backslash escapes" $ do
      fmap rulesToTuples (parseRules "\\\\ -> \\\\;") `shouldBe`
        Right [("\\", "\\")]

    it "reports a helpful error for missing semicolon" $ do
      let result = parseRules "abc -> ABC"
      case result of
        Left msg  -> msg `shouldContainText` "';'"
        Right val -> fail $ "expected Left but got Right: " <> show (rulesToTuples val)

  describe "options parser" $ do
    it "accepts a required rules file" $ do
      let parsed :: ParserResult Options
          parsed = execParserPure defaultPrefs parserInfo ["rules.txt"]
      case parsed of
        Success opts -> rulesFile opts `shouldBe` "rules.txt"
        _            -> fail $ "expected Success, got: " <> show parsed

    it "fails when no rules file provided" $ do
      let parsed :: ParserResult Options
          parsed = execParserPure defaultPrefs parserInfo []
      parsed `shouldSatisfy` isFailureResult

    it "parses optional input string" $ do
      let parsed :: ParserResult Options
          parsed = execParserPure defaultPrefs parserInfo ["rules.txt", "--input", "abc"]
      case parsed of
        Success opts -> inputString opts `shouldBe` Just "abc"
        _            -> fail $ "expected Success, got: " <> show parsed

    it "parses non-negative max steps" $ do
      let parsed :: ParserResult Options
          parsed = execParserPure defaultPrefs parserInfo ["rules.txt", "--max-steps", "2"]
      case parsed of
        Success opts -> maxSteps opts `shouldBe` Just 2
        _            -> fail $ "expected Success, got: " <> show parsed

    it "rejects negative max steps" $ do
      let parsed :: ParserResult Options
          parsed = execParserPure defaultPrefs parserInfo ["rules.txt", "--max-steps", "-1"]
      parsed `shouldSatisfy` isFailureResult

  describe "renderTraceLines" $ do
    it "numbers each rewrite step" $ do
      let rules = [Rule "a" "b", Rule "b" "c"]
      let rendered = renderTraceLines rules "a"
      rendered `shouldBe`
        ["step 0: a", "step 1: b", "step 2: c"]

  describe "renderTraceLinesLimited" $ do
    it "caps the number of reported steps" $ do
      let rules = [Rule "a" "aa"]
      let rendered = renderTraceLinesLimited (Just 1) rules "a"
      rendered `shouldBe` ["step 0: a", "step 1: aa"]

    it "matches renderTraceLines when unlimited" $ do
      let rules = [Rule "a" "aa"]
      take 3 (renderTraceLinesLimited Nothing rules "a") `shouldBe`
        take 3 (renderTraceLines rules "a")

  describe "append-bar example" $ do
    appendRules <- runIO $ do
      contents <- TIO.readFile "examples/append-bar.rules"
      case parseRules contents of
        Left err    -> fail ("failed to parse append-bar.rules: " <> T.unpack err)
        Right rules -> pure rules

    let finalState input = last (trace appendRules input)

    it "handles the empty string and a few concrete inputs" $ do
      finalState "" `shouldBe` "|"
      finalState "1" `shouldBe` "1|"
      finalState "1111111" `shouldBe` "1111111|"

    it "produces a trace that ends with a bar" $ do
      let steps = renderTraceLines appendRules "111"
      last steps `shouldBe` "step 5: 111|"

    prop "appends exactly one bar" $ appendBarProperty appendRules
  where
    appendBarProperty :: Rules Char -> Property
    appendBarProperty rules =
      let maxLen = 16
      in forAll (chooseInt (0, maxLen)) $ \n ->
        let input  = replicate n '1'
            output = last (trace rules input)
        in output === replicate n '1' ++ "|"

rulesToTuples :: Rules Char -> [([Char], [Char])]
rulesToTuples = fmap $ \(Rule lhs rhs) -> (lhs, rhs)

shouldContainText :: Text -> Text -> IO ()
shouldContainText haystack needle
  | needle `T.isInfixOf` haystack = pure ()
  | otherwise = fail $ "expected to find substring `" <> T.unpack needle <> "` in `" <> T.unpack haystack <> "`"

isFailureResult :: ParserResult a -> Bool
isFailureResult Failure{} = True
isFailureResult _         = False
