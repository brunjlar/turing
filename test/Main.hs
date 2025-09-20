module Main (main) where

import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hspec      (testSpec)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  hspecTests <- testSpec "MyLib" unitSpecs
  defaultMain $ testGroup "turing" [hspecTests, propertyTests]

unitSpecs :: Spec
unitSpecs = pure ()

propertyTests :: TestTree
propertyTests =
  testGroup
    "quickcheck"
    []
