{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module Calc.CalcValue.MExpNotation (
    MExpNotation
) where

import Calc.CalcValue.Base ( CalcValue(..) )
import Calc.CalcValue.ExpNotation ( ExpNotation(..) )

type MExpNotation = Maybe ExpNotation

instance CalcValue MExpNotation where
    dot :: MExpNotation -> MExpNotation
    dot = fmap dot

    addDigit :: MExpNotation -> Int -> MExpNotation
    addDigit Nothing _ = Nothing
    addDigit (Just dv) a = Just (addDigit dv a)

    display :: MExpNotation -> String
    display Nothing = "Error"
    display (Just dv) = display dv

instance Num MExpNotation where
    (+) :: MExpNotation -> MExpNotation -> MExpNotation
    Nothing + _ = Nothing
    _ + Nothing = Nothing
    Just dv1 + Just dv2 = Just (dv1 + dv2)

    (-) :: MExpNotation -> MExpNotation -> MExpNotation
    Nothing - _ = Nothing
    _ - Nothing = Nothing
    Just dv1 - Just dv2 = Just (dv1 - dv2)

    (*) :: MExpNotation -> MExpNotation -> MExpNotation
    Nothing * _ = Nothing
    _ * Nothing = Nothing
    Just dv1 * Just dv2 = Just (dv1 * dv2)

    abs :: MExpNotation -> MExpNotation
    abs = fmap abs

    signum :: MExpNotation -> MExpNotation
    signum = fmap signum

    fromInteger :: Integer -> MExpNotation
    fromInteger a = Just (fromInteger a)

instance Fractional MExpNotation where
    (/) :: MExpNotation -> MExpNotation -> MExpNotation
    Nothing / _ = Nothing
    _ / Nothing = Nothing
    _ / (Just 0) = Nothing
    (Just dv1) / (Just dv2) = Just (dv1 / dv2)

    fromRational :: Rational -> MExpNotation
    fromRational a = Just (fromRational a)