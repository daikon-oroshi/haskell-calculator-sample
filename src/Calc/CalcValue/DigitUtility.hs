{-# LANGUAGE InstanceSigs #-}

module Calc.CalcValue.DigitUtility
    (
        IsDigits (..),
        addDigitToLast
    ) where

class IsDigits a where
    -- |
    -- 桁数の計算
    numOfDigits :: a -> Int
    -- |
    -- 末尾の数字を取得
    getLastDigits :: a -> Int
    -- |
    -- 先頭から n 桁を取得する。
    takeTop :: Int -> a -> a

instance IsDigits Int where
    numOfDigits :: Int -> Int
    numOfDigits x
        | x == 0 = 1
        | x > 0 = 1 + floor (logBase (10::Double) $ realToFrac x)
        | otherwise = numOfDigits $ -x

    getLastDigits :: Int -> Int
    getLastDigits x = mod (abs x) 10

    takeTop :: Int -> Int -> Int
    takeTop n x
        | n < 0 = 0
        | numOfDigits x <= n = x
        | otherwise = x `div` 10^(numOfDigits x - n)

addDigitToLast :: Int -> Int -> Int
addDigitToLast 0 digit = digit
addDigitToLast x digit = x * 10 + digit