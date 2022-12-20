{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DispValue
    (
        DispVal,
        toNumber,
        addDigit,
        dot,
        zeroDispVal,
        unitDispVal
    ) where

data DispVal = DispVal {
    is_positive :: Bool,
    has_dot :: Bool,
    integer_part :: Int,
    decimal_part :: Int
}

limitOfDigit :: Int
limitOfDigit = 12

zeroDispVal :: DispVal
zeroDispVal = DispVal {
    is_positive = True,
    has_dot = False,
    integer_part = 0,
    decimal_part = 0
}

unitDispVal :: DispVal
unitDispVal = DispVal {
    is_positive = True,
    has_dot = False,
    integer_part = 1,
    decimal_part = 0
}

instance Num DispVal where
    (+) :: DispVal -> DispVal -> DispVal
    dv_1 + dv_2 = fromNumber $ toNumber dv_1 + toNumber dv_2

    (-) :: DispVal -> DispVal -> DispVal
    dv_1 - dv_2 = fromNumber $ toNumber dv_1 - toNumber dv_2

    (*) :: DispVal -> DispVal -> DispVal
    dv_1 * dv_2 = fromNumber $ toNumber dv_1 * toNumber dv_2

    abs :: DispVal -> DispVal
    abs dv = dv {is_positive = True}

    signum :: DispVal -> DispVal
    signum dv
        | integer_part dv == 0 && decimal_part dv == 0 = zeroDispVal
        | is_positive dv = unitDispVal
        | otherwise = unitDispVal {is_positive = False}

    fromInteger :: Integer -> DispVal
    fromInteger x = DispVal {
        is_positive = x > 0,
        has_dot = False,
        integer_part = fromInteger (mod x 10^limitOfDigit) :: Int,
        decimal_part = 0
    }

instance Fractional DispVal where
    (/) :: DispVal -> DispVal -> DispVal
    dv_1 / dv_2 = fromNumber $ toNumber dv_1 / toNumber dv_2
    fromRational :: Rational -> DispVal
    fromRational = fromNumber

instance Show DispVal where
    show :: DispVal -> String
    show dv
        | integer_part dv == 0 && decimal_part dv == 0 = "0"
        | otherwise = concat
            [
                if is_positive dv then "" else "-",
                show (integer_part dv),
                if has_dot dv then "." else "",
                if decimal_part dv  == 0
                    then ""
                    else show (decimal_part dv)
            ]

instance Eq DispVal where
    (==) :: DispVal -> DispVal -> Bool
    dv_1 == dv_2 = toNumber dv_1 == toNumber dv_2

-- 桁数
numOfDigits :: Int -> Int
numOfDigits x
    | x == 0 = 1
    | x > 0 = ceiling $ logBase (10::Double) $ realToFrac x
    | otherwise = numOfDigits $ -x

toNumber :: DispVal -> Float
toNumber dval =
    if is_positive dval then 1 else -1
        * (
            fromIntegral (integer_part dval)
            + (
                fromIntegral (decimal_part dval)
                / 10 ^ numOfDigits (decimal_part dval)
            )
        )

fromNumber :: (RealFrac a) => a -> DispVal
fromNumber d =
    DispVal {
        is_positive = d > 0,
        has_dot = abs (snd (properFraction d)) > 0,
        integer_part = floor $ abs d,
        decimal_part = floor $ abs (snd (properFraction d)) * 10^limitOfDigit
    }

addDigitToLast :: Int -> Int -> Int
addDigitToLast s t
    | s == 0 = t
    | otherwise = s * 10 + t

addDigit :: DispVal -> Int -> DispVal
addDigit val a =
    if has_dot val
        then val {
            decimal_part = addDigitToLast (decimal_part val) a
        }
        else val {
            integer_part = addDigitToLast (integer_part val) a
        }

dot :: DispVal -> DispVal
dot val
    | not $ has_dot val
        = val {
            has_dot = True
        }
    | has_dot val && decimal_part val == 0
        = val {
            has_dot = False
        }
    | otherwise = val