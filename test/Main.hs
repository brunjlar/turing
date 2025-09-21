{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Hspec       (Spec, describe, it, runIO, shouldBe,
                                   shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (testSpec)

import           Data.Char        (intToDigit)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Numeric          (showIntAtBase)

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

  describe "duplicate example" $ do
    duplicateRules <- runIO $ do
      contents <- TIO.readFile "examples/duplicate.rules"
      case parseRules contents of
        Left err    -> fail ("failed to parse duplicate.rules: " <> T.unpack err)
        Right rules -> pure rules

    let finalDuplicate input = last (trace duplicateRules input)

    it "duplicates the empty string and a few concrete inputs" $ do
      finalDuplicate "" `shouldBe` "|"
      finalDuplicate "1" `shouldBe` "1|1"
      finalDuplicate "111" `shouldBe` "111|111"

    it "produces a symmetric split in the trace" $ do
      let steps = renderTraceLines duplicateRules "111"
      last steps `shouldBe` "step 19: 111|111"

    prop "duplicates unary inputs" $ duplicateProperty duplicateRules

  describe "unary multiply example" $ do
    multiplyRules <- runIO $ do
      contents <- TIO.readFile "examples/unary-multiply.rules"
      case parseRules contents of
        Left err    -> fail ("failed to parse unary-multiply.rules: " <> T.unpack err)
        Right rules -> pure rules

    let finalMultiply input = last (trace multiplyRules input)

    it "handles zero operands" $ do
      finalMultiply "*" `shouldBe` ""
      finalMultiply "*11" `shouldBe` ""
      finalMultiply "111*" `shouldBe` ""

    it "multiplies small concrete pairs" $ do
      finalMultiply "1*1" `shouldBe` "1"
      finalMultiply "11*111" `shouldBe` "111111"
      finalMultiply "111*11" `shouldBe` "111111"

    prop "produces unary products" $ multiplyProperty multiplyRules

  describe "binary increment example" $ do
    incrementRules <- runIO $ do
      contents <- TIO.readFile "examples/binary-increment.rules"
      case parseRules contents of
        Left err    -> fail ("failed to parse binary-increment.rules: " <> T.unpack err)
        Right rules -> pure rules

    let finalIncrement input = last (trace incrementRules input)
        incremented input    = stripGuard (finalIncrement input)

    it "produces traces that end with the guard" $ do
      finalIncrement "0" `shouldSatisfy` endsWithBar
      finalIncrement "101" `shouldSatisfy` endsWithBar

    it "increments specific binary samples" $ do
      incremented "0" `shouldBe` "1"
      incremented "1" `shouldBe` "10"
      incremented "10" `shouldBe` "11"
      incremented "11" `shouldBe` "100"
      incremented "1011101010100" `shouldBe` "1011101010101"
      incremented "11111" `shouldBe` "100000"

    prop "increments canonical binary strings" $ binaryIncrementProperty incrementRules

  describe "unary to binary example" $ do
    unaryBinaryRules <- runIO $ do
      contents <- TIO.readFile "examples/unary-to-binary.rules"
      case parseRules contents of
        Left err    -> fail ("failed to parse unary-to-binary.rules: " <> T.unpack err)
        Right rules -> pure rules

    let finalBinary input = stripSentinel (last (trace unaryBinaryRules input))

    it "converts the documented cases" $ do
      finalBinary "" `shouldBe` "0"
      finalBinary "1" `shouldBe` "1"
      finalBinary "11" `shouldBe` "10"
      finalBinary "111" `shouldBe` "11"
      finalBinary "1111" `shouldBe` "100"
      finalBinary "1111111111" `shouldBe` "1010"

    prop "matches binary encoding for small unary lengths" $
      unaryToBinaryProperty unaryBinaryRules

  where
    duplicateProperty :: Rules Char -> Property
    duplicateProperty rules =
      let maxLen = 12
      in forAll (chooseInt (0, maxLen)) $ \n ->
        let input  = replicate n '1'
            output = last (trace rules input)
        in output === input ++ "|" ++ input

    multiplyProperty :: Rules Char -> Property
    multiplyProperty rules =
      let maxLen = 6
      in forAll (chooseInt (0, maxLen)) $ \a ->
           forAll (chooseInt (0, maxLen)) $ \b ->
             let left   = replicate a '1'
                 right  = replicate b '1'
                 input  = left ++ "*" ++ right
                 output = last (trace rules input)
             in output === replicate (a * b) '1'

    appendBarProperty :: Rules Char -> Property
    appendBarProperty rules =
      let maxLen = 16
      in forAll (chooseInt (0, maxLen)) $ \n ->
        let input  = replicate n '1'
            output = last (trace rules input)
        in output === replicate n '1' ++ "|"

    binaryIncrementProperty :: Rules Char -> Property
    binaryIncrementProperty rules =
      let maxVal = 512
      in forAll (chooseInt (0, maxVal)) $ \n ->
        let input    = toBinary n
            expected = toBinary (n + 1)
            output   = stripGuard (last (trace rules input))
        in output === expected

    unaryToBinaryProperty :: Rules Char -> Property
    unaryToBinaryProperty rules =
      let maxLen = 10
      in forAll (chooseInt (0, maxLen)) $ \n ->
        let input  = replicate n '1'
            output = stripSentinel (last (trace rules input))
        in output === binaryString n


rulesToTuples :: Rules Char -> [([Char], [Char])]
rulesToTuples = fmap $ \(Rule lhs rhs) -> (lhs, rhs)

shouldContainText :: Text -> Text -> IO ()
shouldContainText haystack needle
  | needle `T.isInfixOf` haystack = pure ()
  | otherwise = fail $ "expected to find substring `" <> T.unpack needle <> "` in `" <> T.unpack haystack <> "`"

isFailureResult :: ParserResult a -> Bool
isFailureResult Failure{} = True
isFailureResult _         = False

stripGuard :: String -> String
stripGuard xs = case reverse xs of
  ('|' : rest) -> reverse rest
  _            -> xs

endsWithBar :: String -> Bool
endsWithBar xs = not (null xs) && last xs == '|'

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse (go n)
  where
    go 0 = []
    go k = let (q, r) = k `quotRem` 2
               bit    = if r == 0 then '0' else '1'
           in bit : go q

binaryString :: Int -> String
binaryString n
  | n == 0    = "0"
  | otherwise = showIntAtBase 2 intToDigit n ""

stripSentinel :: String -> String
stripSentinel = takeWhile (/= '#')
