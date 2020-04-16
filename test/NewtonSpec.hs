module NewtonSpec where

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "Tests for Newton module" $ do
    it "2 + 2 = 4" $ 2 + 2 `shouldBe` 4
    it "reverse . reverse = id" $
      property $ \xs -> reverse (reverse xs) == (xs :: [Int])
