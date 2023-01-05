module MExpNotationSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, Expectation )
import Calc.CalcValue.MExpNotation (
        MExpNotation
    )

import Calc.CalcValue (
        display,
        addDigit, CalcValue (addDigit)
    )

testPlus :: (MExpNotation, MExpNotation) -> String -> Expectation
testPlus (val1, val2) _exp = display (val1 + val2) `shouldBe` _exp

testDisplay :: MExpNotation -> String -> Expectation
testDisplay val _exp = display val `shouldBe` _exp

testAddDigit :: (MExpNotation, Int) -> String -> Expectation
testAddDigit (dv, num) _epx = display (addDigit dv num) `shouldBe` _epx

testProd :: (MExpNotation, MExpNotation) -> String -> Expectation
testProd (dv1, dv2) _exp =
    display (dv1 * dv2) `shouldBe` _exp

testDiv :: (MExpNotation, MExpNotation) -> String -> Expectation
testDiv (dv1, dv2) _exp =
    display (dv1 / dv2) `shouldBe` _exp

tests :: [(String, [(String, Expectation)])]
tests = [
        ("表示(display)のテスト", [
            ("Nothing", testDisplay Nothing "Error")
            , ("1", testDisplay (Just 1) "1")
            , ("nagate 1", testDisplay (-1) "-1")
            , ("-0.18", testDisplay (-0.18)
             "-0.18")
        ])
        , ("addDigitのテスト",[
            ("Nothingに追加", testAddDigit (Nothing, 3) "Error")
            , ("1に追加", testAddDigit (1, 1) "11")
        ] )
        , ("掛け算のテスト", [
            ("5 * 9", testProd (5, 9) "45")
            , ("5 * Nothin", testProd (5, Nothing) "Error")
            , ("Nothing * 9", testProd (Nothing, 9) "Error")
            , ("Nothing * Nothing", testProd (Nothing, Nothing) "Error")
        ])
        , ("足し算のテスト", [
            ("0と1の足し算", testPlus (0, 1) "1")
            , ("Nothing + 1", testPlus (Nothing, 1) "Error")
            , ("0 + Nothing", testPlus (0, Nothing) "Error")
            , ("Nothing + Nothing", testPlus (Nothing, Nothing) "Error")
        ])
        , ("割り算のテスト", [
            ("1 / 2", testDiv (1, 2) "0.5")
            , ("1 / 0", testDiv (1, 0) "Error")
            , ("Nothing / 2", testDiv (Nothing, 1) "Error")
            , ("1 / Nothing", testDiv (1, Nothing) "Error")
            , ("Nothing / Nothing", testDiv (Nothing, Nothing) "Error")
        ])
    ]

spec :: Spec
spec = do
    mapM_ (\(n, p) ->
            describe n $ do
                mapM_ (uncurry it) p
        ) tests