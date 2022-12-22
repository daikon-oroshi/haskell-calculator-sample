module CalcValue (
    CalcValue(..),
    Operation (..)
) where

data Operation = Plus | Sub | Prod | Div deriving (Show, Eq)

class (Num a, Fractional a, Show a) => CalcValue a where
    addDigit :: a -> Int -> a
    dot:: a -> a
    calculate :: a -> a -> Maybe Operation -> a
    calculate _ _ Nothing = 0
    calculate dv1 dv2 (Just x)
        | x == Plus = dv1 + dv2
        | x == Sub = dv1 - dv2
        | x == Prod = dv1 * dv2
        | otherwise = dv1 / dv2