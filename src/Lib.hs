module Lib
    (
        DispVal,
        toDispStr,
        toNumber
    ) where

data DispVal = DispVal {
    is_positive :: Bool,
    integer_part :: Int,
    decimal_part :: Int
}

toDispStr :: DispVal -> String
toDispStr dval
    | integer_part dval == 0 && decimal_part dval == 0 = "0"
    | otherwise =
        if is_positive dval then "" else "-"
            ++ show (integer_part dval)
            ++ if decimal_part dval == 0
                then ""
                else "." ++ show (decimal_part dval)

numOfDigit :: Int -> Int
numOfDigit x
    | x == 0 = 1
    | x > 0 = 1
    | otherwise = numOfDigit $ -x

toNumber :: DispVal -> Double
toNumber dval =
    if is_positive dval then 1 else -1
        * (
            fromIntegral (integer_part dval)
            + (
                fromIntegral (decimal_part dval)
                / 10 ^ numOfDigit (decimal_part dval)
            )
        )
