{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DispValue
    (
        DispVal (..),
        toNumber,
        dot,
        addDigitToLast,
        zeroDispVal,
        unitDispVal,
        numOfDigits,
    ) where

import Utility (IsDigits(..))
import Data.Maybe ( isNothing, isJust )
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
    dv_1 + dv_2 = fromNumber (toNumber dv_1 + toNumber dv_2)

    (-) :: DispVal -> DispVal -> DispVal
    dv_1 - dv_2 = fromNumber $ toNumber dv_1 - toNumber dv_2

    (*) :: DispVal -> DispVal -> DispVal
    dv_1 * dv_2 = fromNumber $ toNumber dv_1 * toNumber dv_2

    abs :: DispVal -> DispVal
    abs dv = dv {_significand = abs (_significand dv)}

    signum :: DispVal -> DispVal
    signum dv
        | _significand dv == 0 = zeroDispVal
        | _significand dv > 0 = unitDispVal
        | otherwise = unitDispVal {_significand = -1}

    fromInteger :: Integer -> DispVal
    fromInteger x = takeTop limitOfDigit DispVal {
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

instance IsDigits DispVal where
    numOfDigits :: DispVal -> Int
    numOfDigits DispVal {_exponent = Nothing, _significand = x}
        = numOfDigits x
    numOfDigits DispVal {_exponent = Just e, _significand = x}
        = max (e + 1) (numOfDigits x)

    getLastDigits :: DispVal -> Int
    getLastDigits = getLastDigits . _significand

    addDigitToLast :: DispVal -> Int -> DispVal
    addDigitToLast dv a
        | numOfDigits dv > limitOfDigit = dv
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
            DispVal {
                _exponent = fmap (+1) (_exponent dv),
                _significand = addDigitToLast (_significand dv) a
            }

    takeTop :: Int -> DispVal -> DispVal
    takeTop
        n DispVal {
            _exponent = Nothing,
            _significand = x
        }
        = DispVal {
            _exponent = Nothing,
            _significand = takeTop n x
        }
    takeTop n DispVal {
            _exponent = Just e,
            _significand = x
        }
        | e < numOfDigits x
            = let
                deleted_ditit_num = max 0 (numOfDigits x - n)
            in
                normalize DispVal {
                    _exponent = Just $ max 0 (e - deleted_ditit_num),
                    _significand = takeTop n x
                }
        | otherwise
            = let
                deleted_ditit_num = max 0 (e - n)
            in
                normalize DispVal {
                    _exponent = Just (e - deleted_ditit_num),
                    _significand = takeTop (numOfDigits x - deleted_ditit_num) x
                }

toNumber :: DispVal -> Double
toNumber DispVal {_exponent = Nothing, _significand = x}
    = fromIntegral x
toNumber DispVal {_exponent = Just e, _significand = x}
    = fromIntegral x / (10^e)

fromNumber :: (RealFrac a) => a -> DispVal
fromNumber d =
    let
        sign = if d >= 0 then 1 else -1
        int_part_digits = numOfDigits ((floor $ abs d)::Int)
        e = max 0 $ limitOfDigit - int_part_digits
    in
        takeTop limitOfDigit $ normalize DispVal {
            _exponent = if e == 0 then Nothing else Just e,
            _significand = sign * floor (abs d * 10^e)
        }

dot :: DispVal -> DispVal
dot DispVal {_exponent = Nothing, _significand = x}
    = DispVal {_exponent = Just 0, _significand = x}
dot dv = dv

-- |
-- 小数点以下の末尾0削除
normalize :: DispVal -> DispVal
normalize
    DispVal {
        _exponent = Nothing,
        _significand = x
    }
    = DispVal {
        _exponent = Nothing,
        _significand = x
    }
normalize
    DispVal {
        _exponent = Just 0,
        _significand = x
    }
    = DispVal {
        _exponent = Nothing,
        _significand = x
    }
normalize
    DispVal {
        _exponent = Just e,
        _significand = 0
    }
    = DispVal {
        _exponent = Just e,
        _significand = 0
    }
normalize
    DispVal {
        _exponent = Just e,
        _significand = x
    }
    | getLastDigits x /= 0
        = DispVal {
            _exponent = Just e,
            _significand = x
        }
    | otherwise =
        normalize DispVal {
            _exponent = Just (e - 1),
            _significand = x `div` 10
        }
