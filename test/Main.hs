module Main (main) where

import MyLib (double, greet)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  hspecTests <- testSpec "MyLib" unitSpecs
  defaultMain $ testGroup "turing" [hspecTests, propertyTests]

unitSpecs :: Spec
unitSpecs = do
  describe "greet" $ do
    it "wraps a name in a friendly greeting" $
      greet "Ada" `shouldBe` "Hello, Ada!"
  describe "double" $ do
    it "doubles a positive integer" $
      double 7 `shouldBe` 14

propertyTests :: TestTree
propertyTests =
  testGroup
    "quickcheck"
    [ QC.testProperty "double matches addition" $ \(n :: Int) ->
        double n == n + n
    , QC.testProperty "double always yields an even result" $ \(n :: Int) ->
        even (double n)
    ]
