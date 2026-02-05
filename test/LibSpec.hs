module LibSpec (spec) where

import Lib (add)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "add" $ do
        it "adds two positive numbers" $ do
            add 2 3 `shouldBe` 5

        it "adds positive and negative numbers" $ do
            add 2 (-3) `shouldBe` (-1)

        it "adds two negative numbers" $ do
            add (-2) (-3) `shouldBe` (-5)

        it "is commutative" $
            property $
                \a b -> add a b == add b a

        it "has identity 0" $
            property $
                \a -> add a 0 == a

    describe "external test" $ do
        it "verifies library function from test directory" $ do
            add 75 25 `shouldBe` 100
