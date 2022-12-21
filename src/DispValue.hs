{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DispValue
    (
        DispVal (..),
        toNumber,
        addDigit,
        dot,
        zeroDispVal,
        unitDispVal,
        numOfDigits,
    ) where

import Debug.Trace ( traceShowId )

data DispVal = DispVal {
    _exponent :: Maybe Int,
    _significand :: Int
}

limitOfDigit :: Int
limitOfDigit = 12

zeroDispVal :: DispVal
zeroDispVal = DispVal {
    _exponent = Nothing,
    _significand = 0
}

unitDispVal :: DispVal
unitDispVal = DispVal {
    _exponent = Nothing,
    _significand = 1
}

instance Num DispVal where
    (+) :: DispVal -> DispVal -> DispVal
    dv_1 + dv_2 = normalize $ fromNumber (toNumber dv_1 + toNumber dv_2)

    (-) :: DispVal -> DispVal -> DispVal
    dv_1 - dv_2 = normalize $ fromNumber $ toNumber dv_1 - toNumber dv_2

    (*) :: DispVal -> DispVal -> DispVal
    dv_1 * dv_2 = normalize $ fromNumber $ toNumber dv_1 * toNumber dv_2

    abs :: DispVal -> DispVal
    abs dv = dv {_significand = abs (_significand dv)}

    signum :: DispVal -> DispVal
    signum dv
        | _significand dv == 0 = zeroDispVal
        | _significand dv > 0 = unitDispVal
        | otherwise = unitDispVal {_significand = -1}

    -- TODO: 桁溢れ
    fromInteger :: Integer -> DispVal
    fromInteger x = DispVal {
        _exponent = Nothing,
        _significand = fromIntegral x
    }

instance Fractional DispVal where
    (/) :: DispVal -> DispVal -> DispVal
    dv_1 / dv_2 = fromNumber $ toNumber dv_1 / toNumber dv_2
    fromRational :: Rational -> DispVal
    fromRational = fromNumber

instance Show DispVal where
    show :: DispVal -> String
    show DispVal {
            _exponent = Nothing,
            _significand = x
        } = show x
    show DispVal {
            _exponent = Just e,
            _significand = x
        }
        | x == 0 =
            "0." ++ concat (replicate e "0")
        | otherwise =
            let
                sign_str = if x > 0 then "" else "-"
                abs_x = abs x
                digits = numOfDigits abs_x
            in
                if digits > e
                then
                    let
                        (int_part, deci_part) = splitAt (digits - e) $ show abs_x
                    in sign_str ++ int_part ++ "." ++ deci_part
                else
                    sign_str ++ "0." ++ concat (replicate (e - digits) "0") ++ show abs_x

instance Eq DispVal where
    (==) :: DispVal -> DispVal -> Bool
    dv_1 == dv_2 = toNumber dv_1 == toNumber dv_2

-- 桁数
numOfDigits :: Int -> Int
numOfDigits x
    | x == 0 = 1
    | x > 0 = 1 + floor (logBase (10::Double) $ realToFrac x)
    | otherwise = numOfDigits $ -x

toNumber :: DispVal -> Double
toNumber DispVal {_exponent = Nothing, _significand = x}
    = fromIntegral x
toNumber DispVal {_exponent = Just e, _significand = x}
    = fromIntegral x / (10^e)

-- TODO: 桁溢れ
fromNumber :: (RealFrac a) => a -> DispVal
fromNumber d =
    let
        sign = if d >= 0 then 1 else -1
        int_part_digits = numOfDigits $ floor $ abs d
        e = max 0 $ limitOfDigit - int_part_digits
    in
        normalize DispVal {
            _exponent = if e == 0 then Nothing else Just e,
            _significand = sign * floor (abs d * 10^e)
        }

addDigitToLast :: Int -> Int -> Int
addDigitToLast 0 t = t
addDigitToLast s t = s * 10 + t

addDigit :: DispVal -> Int -> DispVal
addDigit DispVal {_exponent = Nothing, _significand = x} a
    = DispVal {
        _exponent = Nothing,
        _significand = addDigitToLast x a
    }
addDigit DispVal {_exponent = Just e, _significand = 0} 0
    = DispVal {
        _exponent = Just (e + 1),
        _significand = 0
    }
addDigit DispVal {_exponent = Just e, _significand = x} a
    = DispVal {
        _exponent = Just (e + 1),
        _significand = addDigitToLast x a
    }

dot :: DispVal -> DispVal
dot DispVal {_exponent = Nothing, _significand = x}
    = DispVal {_exponent = Just 0, _significand = x}
dot dv = dv

-- 小数点以下の末尾0削除
normalize :: DispVal -> DispVal
normalize DispVal {_exponent = Nothing, _significand = x}
    = DispVal {_exponent = Nothing, _significand = x}
normalize DispVal {_exponent = Just 0, _significand = x}
    = DispVal {_exponent = Nothing, _significand = x}
normalize DispVal {_exponent = e, _significand = 0}
    = DispVal {_exponent = e, _significand = 0}
normalize DispVal {_exponent = Just e, _significand = x}
    | -9 <= x && x <= 9 = DispVal {_exponent = Just e, _significand = x}
    | otherwise =
        let
            sign = signum x
            abs_x = abs x
            _tail = mod abs_x 10
        in
            if _tail /= 0 then
                DispVal {_exponent = Just e, _significand = x}
            else
                normalize DispVal {
                    _exponent = Just (e - 1),
                    _significand = sign * (abs_x `div` 10)
                }
