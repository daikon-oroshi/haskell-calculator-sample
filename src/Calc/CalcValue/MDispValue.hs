{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Calc.CalcValue.MDispValue (
    MDispVal
) where

import Calc.CalcValue.Base ( CalcValue(..) )
import Calc.CalcValue.DispValue ( DispVal(..) )

type MDispVal = Maybe DispVal

instance CalcValue MDispVal where
    dot :: MDispVal -> MDispVal
    dot = fmap dot

    addDigit :: MDispVal -> Int -> MDispVal
    addDigit Nothing _ = Nothing
    addDigit (Just dv) a = Just (addDigit dv a)

    display :: MDispVal -> String
    display Nothing = "Error"
    display (Just dv) = display dv

instance Num MDispVal where
    (+) :: MDispVal -> MDispVal -> MDispVal
    Nothing + _ = Nothing
    _ + Nothing = Nothing
    Just dv1 + Just dv2 = Just (dv1 + dv2)

    (-) :: MDispVal -> MDispVal -> MDispVal
    Nothing - _ = Nothing
    _ - Nothing = Nothing
    Just dv1 - Just dv2 = Just (dv1 - dv2)

    (*) :: MDispVal -> MDispVal -> MDispVal
    Nothing * _ = Nothing
    _ * Nothing = Nothing
    Just dv1 * Just dv2 = Just (dv1 * dv2)

    abs :: MDispVal -> MDispVal
    abs = fmap abs

    signum :: MDispVal -> MDispVal
    signum = fmap signum

    fromInteger :: Integer -> MDispVal
    fromInteger a = Just (fromInteger a)

instance Fractional MDispVal where
    (/) :: MDispVal -> MDispVal -> MDispVal
    Nothing / _ = Nothing
    _ / Nothing = Nothing
    _ / (Just 0) = Nothing
    (Just dv1) / (Just dv2) = Just (dv1 / dv2)

    fromRational :: Rational -> MDispVal
    fromRational a = Just (fromRational a)