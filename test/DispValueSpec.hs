module DispValueSpec (spec) where

import Test.Hspec
import DispValue ( unitDispVal, zeroDispVal )

addDispValue :: Expectation
addDispValue = (zeroDispVal + unitDispVal) `shouldBe` unitDispVal

showUnitDispValue :: Expectation
showUnitDispValue = show unitDispVal `shouldBe` "1"

showMinusUnitDispValue :: Expectation
showMinusUnitDispValue = show (negate unitDispVal) `shouldBe` "-1"

spec :: Spec
spec = do
    describe "addDispValue" $
        it "0 と 1の足し算" addDispValue

    describe "showUnitDispValue" $
        it "1の表示" showUnitDispValue

    describe "showMinusUnitDispValue" $
        it "-1の表示" showMinusUnitDispValue