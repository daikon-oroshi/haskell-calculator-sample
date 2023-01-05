{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Calc.CalcValue.ExpNotation
    (
        ExpNotation (..),
        toNumber,
        dot,
        zeroDispVal,
        unitDispVal,
        numOfDigits,
    ) where

import Calc.CalcValue.DigitUtility (IsDigits(..), addDigitToLast)
import Data.Maybe ( isNothing, isJust )
import Calc.CalcValue.Base ( CalcValue(dot, addDigit, display) )

data ExpNotation = ExpNotation {
    _exponent :: Maybe Int,
    _significand :: Int
}

limitOfDigits :: Int
limitOfDigits = 12

zeroDispVal :: ExpNotation
zeroDispVal = ExpNotation {
    _exponent = Nothing,
    _significand = 0
}

unitDispVal :: ExpNotation
unitDispVal = ExpNotation {
    _exponent = Nothing,
    _significand = 1
}

instance CalcValue ExpNotation where
    dot :: ExpNotation -> ExpNotation
    dot ExpNotation {_exponent = Nothing, _significand = x}
        = ExpNotation {_exponent = Just 0, _significand = x}
    dot dv = dv

    addDigit :: ExpNotation -> Int -> ExpNotation
    addDigit dv a
        | numOfDigits dv > limitOfDigits = dv
        | isNothing (_exponent dv)
            = dv {
                _significand = addDigitToLast (_significand dv) a
            }
        | isJust (_exponent dv) && a == 0 && _significand dv == 0
            = dv {
                _exponent = fmap (+1) (_exponent dv),
                _significand = 0
            }
        | otherwise =
            ExpNotation {
                _exponent = fmap (+1) (_exponent dv),
                _significand = addDigitToLast (_significand dv) a
            }
    display :: ExpNotation -> String
    display ExpNotation {
            _exponent = Nothing,
            _significand = x
        } = show x
    display ExpNotation {
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


instance Num ExpNotation where
    (+) :: ExpNotation -> ExpNotation -> ExpNotation
    dv_1 + dv_2 = fromNumber (toNumber dv_1 + toNumber dv_2)

    (-) :: ExpNotation -> ExpNotation -> ExpNotation
    dv_1 - dv_2 = fromNumber $ toNumber dv_1 - toNumber dv_2

    (*) :: ExpNotation -> ExpNotation -> ExpNotation
    dv_1 * dv_2 = fromNumber $ toNumber dv_1 * toNumber dv_2

    abs :: ExpNotation -> ExpNotation
    abs dv = dv {_significand = abs (_significand dv)}

    signum :: ExpNotation -> ExpNotation
    signum dv
        | _significand dv == 0 = zeroDispVal
        | _significand dv > 0 = unitDispVal
        | otherwise = unitDispVal {_significand = -1}

    fromInteger :: Integer -> ExpNotation
    fromInteger x = takeTop limitOfDigits ExpNotation {
        _exponent = Nothing,
        _significand = fromIntegral x
    }

instance Fractional ExpNotation where
    (/) :: ExpNotation -> ExpNotation -> ExpNotation
    dv_1 / dv_2 = fromNumber $ toNumber dv_1 / toNumber dv_2
    fromRational :: Rational -> ExpNotation
    fromRational = fromNumber

instance Eq ExpNotation where
    (==) :: ExpNotation -> ExpNotation -> Bool
    dv_1 == dv_2 = toNumber dv_1 == toNumber dv_2

instance IsDigits ExpNotation where
    numOfDigits :: ExpNotation -> Int
    numOfDigits ExpNotation {_exponent = Nothing, _significand = x}
        = numOfDigits x
    numOfDigits ExpNotation {_exponent = Just e, _significand = x}
        = max (e + 1) (numOfDigits x)

    getLastDigits :: ExpNotation -> Int
    getLastDigits = getLastDigits . _significand

    takeTop :: Int -> ExpNotation -> ExpNotation
    takeTop
        n ExpNotation {
            _exponent = Nothing,
            _significand = x
        }
        = ExpNotation {
            _exponent = Nothing,
            _significand = takeTop n x
        }
    takeTop n ExpNotation {
            _exponent = Just e,
            _significand = x
        }
        | e < numOfDigits x
            = let
                deleted_ditit_num = max 0 (numOfDigits x - n)
            in
                normalize ExpNotation {
                    _exponent = Just $ max 0 (e - deleted_ditit_num),
                    _significand = takeTop n x
                }
        | otherwise
            = let
                deleted_ditit_num = max 0 (e - n)
            in
                normalize ExpNotation {
                    _exponent = Just (e - deleted_ditit_num),
                    _significand = takeTop (numOfDigits x - deleted_ditit_num) x
                }

toNumber :: ExpNotation -> Double
toNumber ExpNotation {_exponent = Nothing, _significand = x}
    = fromIntegral x
toNumber ExpNotation {_exponent = Just e, _significand = x}
    = fromIntegral x / (10^e)

fromNumber :: (RealFrac a) => a -> ExpNotation
fromNumber d =
    let
        sign = if d >= 0 then 1 else -1
        int_part_digits = numOfDigits ((floor $ abs d)::Int)
        e = max 0 $ limitOfDigits - int_part_digits
    in
        takeTop limitOfDigits $ normalize ExpNotation {
            _exponent = if e == 0 then Nothing else Just e,
            _significand = sign * floor (abs d * 10^e)
        }

-- |
-- 小数点以下の末尾0削除
normalize :: ExpNotation -> ExpNotation
normalize
    ExpNotation {
        _exponent = Nothing,
        _significand = x
    }
    = ExpNotation {
        _exponent = Nothing,
        _significand = x
    }
normalize
    ExpNotation {
        _exponent = Just 0,
        _significand = x
    }
    = ExpNotation {
        _exponent = Nothing,
        _significand = x
    }
normalize
    ExpNotation {
        _exponent = Just e,
        _significand = 0
    }
    = ExpNotation {
        _exponent = Just e,
        _significand = 0
    }
normalize
    ExpNotation {
        _exponent = Just e,
        _significand = x
    }
    | getLastDigits x /= 0
        = ExpNotation {
            _exponent = Just e,
            _significand = x
        }
    | otherwise =
        normalize ExpNotation {
            _exponent = Just (e - 1),
            _significand = x `div` 10
        }
