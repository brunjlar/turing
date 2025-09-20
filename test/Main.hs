{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Test.Hspec       (Spec, describe, it, shouldBe)
import           Test.Tasty       (defaultMain, testGroup)
import           Test.Tasty.Hspec (testSpec)

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Rewrite          (Rule (..), Rules)
import           Rewrite.Parser   (parseRules)

main :: IO ()
main = do
  hspecTests <- testSpec "parser" unitSpecs
  defaultMain $ testGroup "turing" [hspecTests]

unitSpecs :: Spec
unitSpecs = describe "parseRules" $ do
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

rulesToTuples :: Rules Char -> [([Char], [Char])]
rulesToTuples = fmap $ \(Rule lhs rhs) -> (lhs, rhs)

shouldContainText :: Text -> Text -> IO ()
shouldContainText haystack needle
  | needle `T.isInfixOf` haystack = pure ()
  | otherwise = fail $ "expected to find substring `" <> T.unpack needle <> "` in `" <> T.unpack haystack <> "`"
