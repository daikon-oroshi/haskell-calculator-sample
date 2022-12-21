{-# LANGUAGE GADTs, InstanceSigs #-}
module CalcState (
    CalcState,
    ICalcStep(..),
    csStep,
    csCurrentVal,
    initialState,
    Operation (..)
) where

import qualified DispValue as Dv
import DispValue (zeroDispVal)

data FirstInputStep = FirstInputStep deriving Show
data OperationSelectedStep = OperationSelectedStep deriving Show
data SecondInputStep = SecondInputStep deriving Show
data ResultStep = ResultStep deriving Show
data CalcStep = forall s. (ICalcStep s, Show s) => CalcStep s
instance Show CalcStep where
    show (CalcStep s) = "CalcStep" ++ show s

data Operation = Plus | Sub | Prod | Div deriving (Show, Eq)

data CalcState = CalcState {
    csStep :: CalcStep,
    csCurrentVal :: Dv.DispVal,
    csPrevVal :: Dv.DispVal,
    csOperation :: Maybe Operation
}

initialState :: CalcState
initialState = CalcState {
    csStep = CalcStep FirstInputStep,
    csCurrentVal = zeroDispVal,
    csPrevVal = zeroDispVal,
    csOperation = Nothing
}

class ICalcStep a where
    actionDigit :: a -> CalcState -> Int -> CalcState
    actionDigit _ st d =
        st {
            csCurrentVal = Dv.addDigitToLast (csCurrentVal st) d
        }
    actionDot :: a -> CalcState -> CalcState
    actionDot _ st =
        st {
            csCurrentVal = Dv.dot (csCurrentVal st)
        }
    actionZeroZero :: a -> CalcState -> CalcState
    actionZeroZero _ st =
        st {
            csCurrentVal = Dv.addDigitToLast (
                Dv.addDigitToLast (csCurrentVal st) 0
            ) 0
        }
    actionPm :: a -> CalcState -> CalcState
    actionPm _ st =
        st {
            csCurrentVal = -(csCurrentVal st)
        }
    actionAc :: a -> CalcState -> CalcState
    actionAc _ _ = initialState
    actionC :: a -> CalcState -> CalcState
    actionC _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = zeroDispVal
        }
    actionOperation :: a -> CalcState -> Operation -> CalcState
    actionEq :: a -> CalcState -> CalcState

instance ICalcStep FirstInputStep where
    actionOperation :: FirstInputStep -> CalcState -> Operation -> CalcState
    actionOperation _ st op =
        st {
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: FirstInputStep -> CalcState -> CalcState
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep
        }

instance ICalcStep OperationSelectedStep where
    actionDigit :: OperationSelectedStep -> CalcState -> Int -> CalcState
    actionDigit _ st d =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = Dv.addDigitToLast zeroDispVal d
        }
    actionDot :: OperationSelectedStep -> CalcState -> CalcState
    actionDot _ st =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = Dv.dot zeroDispVal
        }
    actionZeroZero :: OperationSelectedStep -> CalcState -> CalcState
    actionZeroZero _ st =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = Dv.addDigitToLast (
                Dv.addDigitToLast zeroDispVal 0
            ) 0
        }
    actionPm :: OperationSelectedStep -> CalcState -> CalcState
    actionPm _ st = st

    actionOperation :: OperationSelectedStep ->CalcState -> Operation -> CalcState
    actionOperation _ st op =
        st {
            csOperation = Just op
        }
    actionEq :: OperationSelectedStep -> CalcState -> CalcState
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep,
            csOperation = Nothing
        }

instance ICalcStep SecondInputStep where
    actionOperation :: SecondInputStep -> CalcState -> Operation -> CalcState
    actionOperation _ st op =
        st {
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: SecondInputStep -> CalcState -> CalcState
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep,
            csCurrentVal = calc (csPrevVal st) (csCurrentVal st) (csOperation st),
            csPrevVal = zeroDispVal,
            csOperation = Nothing
        }
    actionC :: a -> CalcState -> CalcState
    actionC _ st =
        st {
            csCurrentVal = zeroDispVal
        }

instance ICalcStep ResultStep where
    actionDigit :: ResultStep -> CalcState -> Int -> CalcState
    actionDigit _ st d =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = Dv.addDigitToLast zeroDispVal d
        }
    actionDot :: ResultStep -> CalcState -> CalcState
    actionDot _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = Dv.dot zeroDispVal
        }
    actionZeroZero :: ResultStep -> CalcState -> CalcState
    actionZeroZero _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = Dv.addDigitToLast (
                Dv.addDigitToLast zeroDispVal 0
            ) 0
        }
    actionPm :: ResultStep -> CalcState -> CalcState
    actionPm _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = -(csCurrentVal st)
        }
    actionOperation :: ResultStep -> CalcState -> Operation -> CalcState
    actionOperation _ st op =
        st {
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: ResultStep -> CalcState -> CalcState
    actionEq _ st = st

instance ICalcStep CalcStep where
    actionDigit :: CalcStep -> CalcState -> Int -> CalcState
    actionDigit (CalcStep s) = actionDigit s
    actionDot ::  CalcStep -> CalcState -> CalcState
    actionDot (CalcStep s) = actionDot s
    actionZeroZero :: CalcStep -> CalcState -> CalcState
    actionZeroZero (CalcStep s) = actionZeroZero s
    actionPm :: CalcStep -> CalcState -> CalcState
    actionPm (CalcStep s) = actionPm s
    actionAc :: CalcStep -> CalcState -> CalcState
    actionAc (CalcStep s) = actionAc s
    actionOperation :: CalcStep -> CalcState -> Operation -> CalcState
    actionOperation (CalcStep s) = actionOperation s
    actionC :: CalcStep -> CalcState -> CalcState
    actionC (CalcStep s) = actionC s
    actionEq :: CalcStep -> CalcState -> CalcState
    actionEq (CalcStep s) = actionEq s

calc :: Fractional a => a -> a -> Maybe Operation -> a
calc _ _ Nothing = 0
calc dv1 dv2 (Just x)
    | x == Plus = dv1 + dv2
    | x == Sub = dv1 - dv2
    | x == Prod = dv1 * dv2
    | otherwise = dv1 / dv2