module DispValueSpec (spec) where

import Test.Hspec
import DispValue ( unitDispVal, zeroDispVal, addNumber )

testAddDispValue :: Expectation
testAddDispValue = (zeroDispVal + unitDispVal) `shouldBe` unitDispVal

testShowUnitDispValue :: Expectation
testShowUnitDispValue = show unitDispVal `shouldBe` "1"

testShowMinusUnitDispValue :: Expectation
testShowMinusUnitDispValue = show (negate unitDispVal) `shouldBe` "-1"

testAddNumberToZero :: Expectation
testAddNumberToZero = show (addNumber zeroDispVal 1) `shouldBe` "1"

testAddNumber :: Expectation
testAddNumber = show (addNumber unitDispVal 1) `shouldBe` "11"


spec :: Spec
spec = do
    describe "testAddDispValue" $
        it "0 と 1の足し算" testAddDispValue

    describe "testShowUnitDispValue" $
        it "1の表示" testShowUnitDispValue

    describe "testShowMinusUnitDispValue" $
        it "-1の表示" testShowMinusUnitDispValue

    describe "testAddNumberToZero" $
        it "数字の追加" testAddNumberToZero

    describe "testAddNumber" $
        it "数字の追加" testAddNumber