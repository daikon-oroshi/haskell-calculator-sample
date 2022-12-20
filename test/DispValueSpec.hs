module DispValueSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, Expectation )
import DispValue ( unitDispVal, zeroDispVal, addDigit, DispVal )

testAdd1 :: Expectation
testAdd1 = (zeroDispVal + unitDispVal) `shouldBe` unitDispVal

testShowUnit :: Expectation
testShowUnit = show unitDispVal `shouldBe` "1"

testShowNegateUnit :: Expectation
testShowNegateUnit = show (negate unitDispVal) `shouldBe` "-1"

testAddDigitToZero :: Expectation
testAddDigitToZero = show (addDigit zeroDispVal 1) `shouldBe` "1"

testAddDigit :: Expectation
testAddDigit = show (addDigit unitDispVal 1) `shouldBe` "11"

testProd :: Expectation
testProd =
    show (dv1 * dv2) `shouldBe` "45"
    where
        dv1 = 5 :: DispVal
        dv2 = 9 :: DispVal

tests :: [(String, [(String, Expectation)])]
tests = [
        ("testPlus", [
            ("0と1の足し算", testAdd1)
        ])
        , ("testShow", [
            ("1", testShowUnit)
            , ("nagate 1", testShowNegateUnit)
        ])
        , ("testAddDigit",[
            ("0に追加", testAddDigitToZero)
            , ("1に1を追加", testAddDigit)
        ] )
        , ("testProd", [
            ("5 * 9", testProd)
        ])
    ]

spec :: Spec
spec = do
    mapM_ (\(n, p) ->
            describe n $ do
                mapM_ (uncurry it) p
        ) tests