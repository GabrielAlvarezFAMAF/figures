import Test.
import Pred

import Dibujo 
main :: IO ()
main = hspec $ do
  describe "andP" $ do
    it "returns True if both predicates are True" $
      andP (const True) (const True) 42 `shouldBe` True

    it "returns False if either predicate is False" $
      andP (const True) (const False) 42 `shouldBe` False

  describe "orP" $ do
    it "returns True if either predicate is True" $
      orP (const True) (const False) 42 `shouldBe` True

    it "returns False if both predicates are False" $
      orP (const False) (const False) 42 `shouldBe` Falseimport Test.Hspec

  describe "anyDib" $ do
    it "returns True if any element in the Dibujo satisfies the predicate" $ do
      let dibujo = [1, 2, 3, 4, 5]
      anyDib even dibujo `shouldBe` True

    it "returns False if no element in the Dibujo satisfies the predicate" $ do
      let dibujo = [1, 3, 5, 7, 9]
      anyDib even dibujo `shouldBe` False

    it "returns False for an empty Dibujo" $ do
      let dibujo = []
      anyDib even dibujo `shouldBe` False