module ExpNotationSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, Expectation )
import Calc.CalcValue.ExpNotation (
        unitDispVal
        , zeroDispVal
        , numOfDigits
        , ExpNotation (ExpNotation, _significand, _exponent)
    )
import Calc.CalcValue (
        display,
        addDigit, CalcValue (addDigit)
    )

instance Show ExpNotation where
    show = display

testPlus :: (ExpNotation, ExpNotation) -> ExpNotation -> Expectation
testPlus (val1, val2) _exp = (val1 + val2) `shouldBe` _exp

testDisplay :: ExpNotation -> String -> Expectation
testDisplay val _exp = display val `shouldBe` _exp

testAddDigit :: (ExpNotation, Int) -> String -> Expectation
testAddDigit (dv, num) _epx = display (addDigit dv num) `shouldBe` _epx

testProd :: (ExpNotation, ExpNotation) -> String -> Expectation
testProd (dv1, dv2) _exp =
    display (dv1 * dv2) `shouldBe` _exp

testNumOfDigit :: ExpNotation -> Int -> Expectation
testNumOfDigit val _exp = numOfDigits val `shouldBe` _exp

tests :: [(String, [(String, Expectation)])]
tests = [
        ("testShow", [
            ("1", testDisplay unitDispVal "1")
            , ("-1", testDisplay (unitDispVal {_significand = -1}) "-1")
            , ("nagate 1", testDisplay (-unitDispVal) "-1")
            , ("0.", testDisplay ExpNotation {
                _significand = 0,
                _exponent = Just 0
            } "0.")
            , ("0.00", testDisplay ExpNotation {
                _significand = 0,
                _exponent = Just 2
            } "0.00")
            , ("91.", testDisplay ExpNotation {
                _significand = 91,
                _exponent = Just 0
            } "91.")
            , ("9.1", testDisplay ExpNotation {
                _significand = 91,
                _exponent = Just 1
            } "9.1")
            , ("0.91", testDisplay ExpNotation {
                _significand = 91,
                _exponent = Just 2
            } "0.91")
            , ("-91.", testDisplay ExpNotation {
                _significand = -91,
                _exponent = Just 0
            } "-91.")
            , ("-9.1", testDisplay ExpNotation {
                _significand = -91,
                _exponent = Just 1
            } "-9.1")
            , ("-0.91", testDisplay ExpNotation {
                _significand = -91,
                _exponent = Just 2
            } "-0.91")
        ])
        , ("testShowFromNumber", [
            ("1", testDisplay 1 "1")
            , ("1", testDisplay 0 "0")
            , ("0.91", testDisplay 0.91 "0.91")
            , ("-1", testDisplay (-1) "-1")
            , ("-1.9", testDisplay (-1.9) "-1.9")
            , ("-0.91", testDisplay (-0.91) "-0.91")
        ])
        , ("testAddDigit",[
            ("0に追加", testAddDigit (zeroDispVal, 3) "3")
            , ("1に1を追加", testAddDigit (unitDispVal, 1) "11")
        ] )
        , ("testProd", [
            ("5 * 9", testProd (5, 9) "45")
        ])
        , ("testPlus", [
            ("0と1の足し算", testPlus (zeroDispVal, unitDispVal) unitDispVal)
        ])
        , ("numOfDigit", [
            ("0", testNumOfDigit 0 1)
            , ("1", testNumOfDigit 1 1)
            , ("11", testNumOfDigit 11 2)
            , ("-1", testNumOfDigit (-1) 1)
            , ("-0.09", testNumOfDigit (-0.09) 3)
        ])
        , ("numOfDigits", [
            ("1234567890123", testDisplay 1234567890123 "123456789012")
            , ("0.123456789012", testDisplay 0.123456789012 "0.12345678901")
        ])
    ]

spec :: Spec
spec = do
    mapM_ (\(n, p) ->
            describe n $ do
                mapM_ (uncurry it) p
        ) tests