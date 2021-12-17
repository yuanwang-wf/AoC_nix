-- |
module Y2021.Day8Spec (spec) where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck
import Y2021.Day8 (canBeNine, canBeSix, canBeZero, entry, extendMap, initMap, iterateMap, isSolved, hasNine)

spec :: Spec
spec = do
  describe "Day 8" $ do
    it "canBeZero" $ do
      let m = extendMap $ initMap entry
      canBeZero m "cefabd" `shouldBe` False
      canBeSix m "cefabd" `shouldBe` False
      canBeNine m "cefabd" `shouldBe` True

    it "extend should" $ do
      (extendMap . initMap) entry
        `shouldBe` M.fromList
          [ (S.fromList "ab", S.fromList "cf"),
            (S.fromList "abd", S.fromList "acf"),
            (S.fromList "abef", S.fromList "bcdf"),
            (S.fromList "d", S.fromList "a"),
            (S.fromList "ef", S.fromList "bd")
          ]

    it "should iterateMap" $ do
      let m = extendMap $ initMap entry
          expectedM =
            M.fromList
              [ (S.fromList "ab", S.fromList "cf"),
                (S.fromList "abcd", S.fromList "acfg"),
                (S.fromList "abcdef", S.fromList "abcdfg"),
                (S.fromList "abcef", S.fromList "bcdfg"),
                (S.fromList "abd", S.fromList "acf"),
                (S.fromList "abef", S.fromList "bcdf"),
                (S.fromList "cd", S.fromList "ag"),
                (S.fromList "cdef", S.fromList "abdg"),
                (S.fromList "cef", S.fromList "bdg"),
                (S.fromList "d", S.fromList "a"),
                (S.fromList "ef", S.fromList "bd")
              ]
       in (iterateMap m "cefabd") `shouldBe` expectedM

    it "next step" $ do
     let m =
            M.fromList
              [ (S.fromList "ab", S.fromList "cf"),
                (S.fromList "abcd", S.fromList "acfg"),
                (S.fromList "abcdef", S.fromList "abcdfg"),
                (S.fromList "abcef", S.fromList "bcdfg"),
                (S.fromList "abd", S.fromList "acf"),
                (S.fromList "abef", S.fromList "bcdf"),
                (S.fromList "cd", S.fromList "ag"),
                (S.fromList "cdef", S.fromList "abdg"),
                (S.fromList "cef", S.fromList "bdg"),
                (S.fromList "d", S.fromList "a"),
                (S.fromList "ef", S.fromList "bd")
              ]
     isSolved m "cagedb" `shouldBe` (True, 0)
