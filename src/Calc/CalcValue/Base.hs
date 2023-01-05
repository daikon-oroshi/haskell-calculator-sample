module Calc.CalcValue.Base (
    CalcValue(..),
    Operation (..)
) where

data Operation = Plus | Sub | Prod | Div deriving (Show, Eq)

class (Num a, Fractional a) => CalcValue a where
    -- |
    -- 数字を末尾に追加する
    addDigit :: a -> Int -> a
    -- |
    -- 小数点をつける
    dot:: a -> a
    -- |
    -- 表示する
    display :: a -> String
    -- |
    -- 計算する
    calculate :: a -> a -> Maybe Operation -> a
    calculate _ _ Nothing = 0
    calculate dv1 dv2 (Just x)
        | x == Plus = dv1 + dv2
        | x == Sub = dv1 - dv2
        | x == Prod = dv1 * dv2
        | otherwise = dv1 / dv2