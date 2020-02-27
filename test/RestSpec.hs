module RestSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import KrakenExchange.Rest

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "TBC" $ do
    it "works" $ do
      True `shouldBe` True
