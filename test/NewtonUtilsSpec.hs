module NewtonUtilsSpec where

import           Newton.Utils (root)
import           Test.Hspec

spec :: Spec
spec =
  describe "Tests for NewtonUtils module" $ do
    it "root on example 1" $
      let ans = root 100 (< 1e-7) (\x -> x ^ 2 - 2) (* 2) 123
       in fmap (fmap (\x -> abs (x - 1.414213562) < 1e-9)) ans `shouldBe`
          Just (10, True)
    it "root on example 2" $
      let ans = root 100 (< 1e-12) cos (negate . sin) 1.0
       in fmap (fmap (\x -> abs (x - 1.570796326) < 1e-9)) ans `shouldBe`
          Just (3, True)
    it "root on example 3" $
      root 100 (< 1e-12) cos (negate . sin) 0 `shouldBe` Nothing
