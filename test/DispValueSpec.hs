module DispValueSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, Expectation )
import DispValue (
        unitDispVal
        , zeroDispVal
        , addDigitToLast
        , numOfDigits
        , DispVal (DispVal, _significand, _exponent)
    )

testPlus :: (DispVal, DispVal) -> DispVal -> Expectation
testPlus (val1, val2) _exp = (val1 + val2) `shouldBe` val2

testShow :: DispVal -> String -> Expectation
testShow val _exp = show val `shouldBe` _exp

testAddDigit :: (DispVal, Int) -> String -> Expectation
testAddDigit (dv, num) _epx = show (addDigitToLast dv num) `shouldBe` _epx

testProd :: (DispVal, DispVal) -> String -> Expectation
testProd (dv1, dv2) _exp =
    show (dv1 * dv2) `shouldBe` _exp

testNumOfDigit :: Int -> Int -> Expectation
testNumOfDigit val _exp = numOfDigits val `shouldBe` _exp

tests :: [(String, [(String, Expectation)])]
tests = [
        ("testShow", [
            ("1", testShow unitDispVal "1")
            , ("-1", testShow (unitDispVal {_significand = -1}) "-1")
            , ("nagate 1", testShow (-unitDispVal) "-1")
            , ("0.", testShow DispVal {
                _significand = 0,
                _exponent = Just 0
            } "0.")
            , ("0.00", testShow DispVal {
                _significand = 0,
                _exponent = Just 2
            } "0.00")
            , ("91.", testShow DispVal {
                _significand = 91,
                _exponent = Just 0
            } "91.")
            , ("9.1", testShow DispVal {
                _significand = 91,
                _exponent = Just 1
            } "9.1")
            , ("0.91", testShow DispVal {
                _significand = 91,
                _exponent = Just 2
            } "0.91")
            , ("-91.", testShow DispVal {
                _significand = -91,
                _exponent = Just 0
            } "-91.")
            , ("-9.1", testShow DispVal {
                _significand = -91,
                _exponent = Just 1
            } "-9.1")
            , ("-0.91", testShow DispVal {
                _significand = -91,
                _exponent = Just 2
            } "-0.91")
        ])
        , ("testShowFromNumber", [
            ("1", testShow 1 "1")
            , ("1", testShow 0 "0")
            , ("0.91", testShow 0.91 "0.91")
            , ("-1", testShow (-1) "-1")
            , ("-1.9", testShow (-1.9) "-1.9")
            , ("-0.91", testShow (-0.91) "-0.91")
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
        ])
    ]

spec :: Spec
spec = do
    mapM_ (\(n, p) ->
            describe n $ do
                mapM_ (uncurry it) p
        ) tests