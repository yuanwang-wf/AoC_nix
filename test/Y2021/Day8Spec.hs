-- |

module Y2021.Day8Spec (spec) where

import Y2021.Day8 (entry, initMap, extendMap, canBeZero, canBeNine, canBeSix)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "Day 8" $ do
    it "canBeZero" $ do
      let m = extendMap $ initMap entry
      canBeZero m "cefabd"  `shouldBe` False
      canBeSix m "cefabd"  `shouldBe` False
      canBeNine m "cefabd"  `shouldBe` True

    it "extend should" $ do
      (extendMap . initMap)  entry `shouldBe` M.fromList [(S.fromList "ab", S.fromList  "cf"),
                                                          (S.fromList "abd", S.fromList "acf"),
                                                          (S.fromList "abef", S.fromList "bcdf"),
                                                          (S.fromList "d", S.fromList "a"),
                                                          (S.fromList "ef", S.fromList "bd")]
