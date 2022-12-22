{-# LANGUAGE GADTs, InstanceSigs, RankNTypes #-}
module Calc.CalcState (
    CalcState,
    ICalcStep(..),
    csStep,
    csCurrentVal,
    initialState,
    Operation (..)
) where

import Calc.CalcValue ( CalcValue(..), Operation(..) )

data FirstInputStep = FirstInputStep deriving Show
data OperationSelectedStep = OperationSelectedStep deriving Show
data SecondInputStep = SecondInputStep deriving Show
data ResultStep = ResultStep deriving Show
data CalcStep = forall s. (ICalcStep s, Show s) => CalcStep s
instance Show CalcStep where
    show (CalcStep s) = "CalcStep" ++ show s

data CalcState v = (CalcValue v) => CalcState {
    csStep :: CalcStep,
    csCurrentVal :: v,
    csPrevVal :: v,
    csOperation :: Maybe Operation
}

initialState :: forall v. (CalcValue v) => CalcState v
initialState = CalcState {
    csStep = CalcStep FirstInputStep,
    csCurrentVal = 0,
    csPrevVal = 0,
    csOperation = Nothing
}

class ICalcStep a where
    actionDigit :: (CalcValue v) => a -> CalcState v -> Int -> CalcState v
    actionDigit _ st d =
        st {
            csCurrentVal = addDigit (csCurrentVal st) d
        }
    actionDot :: (CalcValue v) => a -> CalcState v -> CalcState v
    actionDot _ st =
        st {
            csCurrentVal = dot (csCurrentVal st)
        }
    actionZeroZero :: (CalcValue v) => a -> CalcState v -> CalcState v
    actionZeroZero _ st =
        st {
            csCurrentVal = addDigit (
                addDigit (csCurrentVal st) 0
            ) 0
        }
    actionPm :: (CalcValue v) => a -> CalcState v -> CalcState v
    actionPm _ st =
        st {
            csCurrentVal = -(csCurrentVal st)
        }
    actionAc :: (CalcValue v) => a -> CalcState v -> CalcState v
    actionAc _ _ = initialState
    actionC :: (CalcValue v) => a -> CalcState v -> CalcState v
    actionC _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = 0
        }
    actionOperation :: (CalcValue v) => a -> CalcState v -> Operation -> CalcState v
    actionEq :: (CalcValue v) => a -> CalcState v -> CalcState v

instance ICalcStep FirstInputStep where
    actionOperation :: FirstInputStep -> CalcState v -> Operation -> CalcState v
    actionOperation _ st op =
        st {
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: FirstInputStep -> CalcState v -> CalcState v
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep
        }

instance ICalcStep OperationSelectedStep where
    actionDigit :: (CalcValue v) => OperationSelectedStep -> CalcState v -> Int -> CalcState v
    actionDigit _ st d =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = addDigit 0 d
        }
    actionDot :: (CalcValue v) => OperationSelectedStep -> CalcState v -> CalcState v
    actionDot _ st =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = dot 0
        }
    actionZeroZero :: (CalcValue v) => OperationSelectedStep -> CalcState v -> CalcState v
    actionZeroZero _ st =
        st {
            csStep = CalcStep SecondInputStep,
            csPrevVal = csCurrentVal st,
            csCurrentVal = addDigit (
                addDigit 0 0
            ) 0
        }
    actionPm :: OperationSelectedStep -> CalcState v -> CalcState v
    actionPm _ st = st

    actionOperation :: OperationSelectedStep -> CalcState v -> Operation -> CalcState v
    actionOperation _ st op =
        st {
            csOperation = Just op
        }
    actionEq :: OperationSelectedStep -> CalcState v -> CalcState v
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep,
            csOperation = Nothing
        }

instance ICalcStep SecondInputStep where
    actionOperation :: (CalcValue v) => SecondInputStep -> CalcState v -> Operation -> CalcState v
    actionOperation _ st op =
        st {
            csCurrentVal = calculate (csPrevVal st) (csCurrentVal st) (csOperation st),
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: (CalcValue v) => SecondInputStep -> CalcState v -> CalcState v
    actionEq _ st =
        st {
            csStep = CalcStep ResultStep,
            csCurrentVal = calculate (csPrevVal st) (csCurrentVal st) (csOperation st),
            csPrevVal = 0,
            csOperation = Nothing
        }
    actionC :: (CalcValue v) => SecondInputStep -> CalcState v -> CalcState v
    actionC _ st =
        st {
            csCurrentVal = 0
        }

instance ICalcStep ResultStep where
    actionDigit :: (CalcValue v) => ResultStep -> CalcState v -> Int -> CalcState v
    actionDigit _ st d =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = addDigit 0 d
        }
    actionDot :: (CalcValue v) => ResultStep -> CalcState v -> CalcState v
    actionDot _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = dot 0
        }
    actionZeroZero :: (CalcValue v) => ResultStep -> CalcState v -> CalcState v
    actionZeroZero _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = addDigit (
                addDigit 0 0
            ) 0
        }
    actionPm :: (CalcValue v) => ResultStep -> CalcState v -> CalcState v
    actionPm _ st =
        st {
            csStep = CalcStep FirstInputStep,
            csCurrentVal = -(csCurrentVal st)
        }
    actionOperation :: ResultStep -> CalcState v -> Operation -> CalcState v
    actionOperation _ st op =
        st {
            csStep = CalcStep OperationSelectedStep,
            csOperation = Just op
        }
    actionEq :: ResultStep -> CalcState v -> CalcState v
    actionEq _ st = st

instance ICalcStep CalcStep where
    actionDigit :: (CalcValue v) => CalcStep -> CalcState v -> Int -> CalcState v
    actionDigit (CalcStep s) = actionDigit s
    actionDot :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionDot (CalcStep s) = actionDot s
    actionZeroZero :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionZeroZero (CalcStep s) = actionZeroZero s
    actionPm :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionPm (CalcStep s) = actionPm s
    actionAc :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionAc (CalcStep s) = actionAc s
    actionOperation :: (CalcValue v) => CalcStep -> CalcState v -> Operation -> CalcState v
    actionOperation (CalcStep s) = actionOperation s
    actionC :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionC (CalcStep s) = actionC s
    actionEq :: (CalcValue v) => CalcStep -> CalcState v -> CalcState v
    actionEq (CalcStep s) = actionEq s
