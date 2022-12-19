module CalcState (
    CalcState,
    _csStep,
    _csCurrentVal,
    CalcStep,
    ICalcStep,
    FirstInputStep
    --OperationSelectedState,
    --SecondInputState
) where

import qualified DispValue as Dv
import Control.Monad.State.Lazy
import DispValue (zeroDispVal)

data FirstInputStep = FirstInputStep
data OperationSelectedStep = OperationSelectedStep
data SecondInputStep = SecondInputStep
data ResultStep = ResultStep
data CalcStep = forall s. (ICalcStep s) => CalcStep s

data Operation = Plus | Minus | Prod | Div deriving Eq

data CalcState = CalcState {
    _csStep :: CalcStep,
    _csCurrentVal :: Dv.DispVal,
    _csPrevVal :: Dv.DispVal,
    _csOperation :: Maybe Operation
}

initialState :: CalcState
initialState = CalcState {
    _csStep = CalcStep FirstInputStep,
    _csCurrentVal = zeroDispVal,
    _csPrevVal = zeroDispVal,
    _csOperation = Nothing
}

class ICalcStep a where
    onClickNumber :: a -> Int -> State CalcState ()
    onClickNumber _ num = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addNumber (_csCurrentVal st) num
        }
    onClickDot :: a -> State CalcState ()
    onClickDot _ = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addDot (_csCurrentVal st)
        }
    onClickZeroZero :: a -> State CalcState ()
    onClickZeroZero _ = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addNumber (
                Dv.addNumber (_csCurrentVal st) 0
            ) 0
        }
    onClickPm :: a -> State CalcState ()
    onClickPm _ = do
        modify $ \st -> st {
            _csCurrentVal = -(_csCurrentVal st)
        }
    onClickAc :: a -> State CalcState ()
    onClickAc _ = do
        put initialState
    onClickC :: a -> State CalcState ()
    onClickC _ = do
        modify $ \st -> st {
            _csStep = CalcStep FirstInputStep,
            _csCurrentVal = zeroDispVal
        }
    onClickOperation :: a -> Operation -> State CalcState ()
    onClickEqual :: a -> State CalcState ()

instance ICalcStep FirstInputStep where
    onClickOperation :: FirstInputStep -> Operation -> State CalcState ()
    onClickOperation _ op = do
        modify $ \st -> st {
            _csStep = CalcStep OperationSelectedStep,
            _csOperation = Just op
        }
    onClickEqual :: FirstInputStep -> State CalcState ()
    onClickEqual _ = do
        modify $ \st -> st {
            _csStep = CalcStep ResultStep
        }

instance ICalcStep OperationSelectedStep where
    onClickNumber :: a -> Int -> State CalcState ()
    onClickNumber _ num = do
        modify $ \st -> st {
            _csStep = CalcStep SecondInputStep,
            _csPrevVal = _csCurrentVal st,
            _csCurrentVal = Dv.addNumber zeroDispVal num
        }
    onClickDot :: a -> State CalcState ()
    onClickDot _ = do
        modify $ \st -> st {
            _csStep = CalcStep SecondInputStep,
            _csPrevVal = _csCurrentVal st,
            _csCurrentVal = Dv.addDot zeroDispVal
        }
    onClickZeroZero :: a -> State CalcState ()
    onClickZeroZero _ = do
        modify $ \st -> st {
            _csStep = CalcStep SecondInputStep,
            _csPrevVal = _csCurrentVal st,
            _csCurrentVal = Dv.addNumber (
                Dv.addNumber zeroDispVal 0
            ) 0
        }
    onClickPm :: a -> State CalcState ()
    onClickPm _ = return ()

    onClickOperation :: OperationSelectedStep -> Operation -> State CalcState ()
    onClickOperation _ op = do
        modify $ \st -> st {
            _csOperation = Just op
        }
    onClickEqual :: OperationSelectedStep -> State CalcState ()
    onClickEqual _ = do modify $ \st -> st {
        _csStep = CalcStep ResultStep,
        _csOperation = Nothing
    }

instance ICalcStep SecondInputStep where
    onClickOperation :: SecondInputStep -> Operation -> State CalcState ()
    onClickOperation _ op = do
        modify $ \st -> st {
            _csStep = CalcStep OperationSelectedStep,
            _csOperation = Just op
        }
    onClickEqual :: SecondInputStep -> State CalcState ()
    onClickEqual _ = do modify $ \st -> st {
        _csStep = CalcStep ResultStep,
        _csCurrentVal = calc (_csCurrentVal st) (_csPrevVal st) (_csOperation st),
        _csPrevVal = zeroDispVal,
        _csOperation = Nothing
    }

instance ICalcStep ResultStep where
    onClickNumber :: ResultStep -> Int -> State CalcState ()
    onClickNumber _ num = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addNumber zeroDispVal num
        }
    onClickDot :: ResultStep -> State CalcState ()
    onClickDot _ = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addDot zeroDispVal
        }
    onClickZeroZero :: ResultStep -> State CalcState ()
    onClickZeroZero _ = do
        modify $ \st -> st {
            _csCurrentVal = Dv.addNumber (
                Dv.addNumber zeroDispVal 0
            ) 0
        }
    onClickPm :: ResultStep -> State CalcState ()
    onClickPm _ = do
        modify $ \st -> st {
            _csCurrentVal = -(_csCurrentVal st)
        }
    onClickOperation :: ResultStep -> Operation -> State CalcState ()
    onClickOperation _ op = do
        modify $ \st -> st {
            _csStep = CalcStep OperationSelectedStep,
            _csOperation = Just op
        }
    onClickEqual :: ResultStep -> State CalcState ()
    onClickEqual _ = return ()

instance ICalcStep CalcStep where
    onClickNumber :: CalcStep -> Int -> State CalcState ()
    onClickNumber (CalcStep s) = onClickNumber s
    onClickDot ::  CalcStep -> State CalcState ()
    onClickDot (CalcStep s) = onClickDot s
    onClickZeroZero :: CalcStep -> State CalcState ()
    onClickZeroZero (CalcStep s) = onClickZeroZero s
    onClickPm :: CalcStep -> State CalcState ()
    onClickPm (CalcStep s) = onClickPm s
    onClickAc :: CalcStep -> State CalcState ()
    onClickAc (CalcStep s) = onClickAc s
    onClickOperation :: CalcStep -> Operation -> State CalcState ()
    onClickOperation (CalcStep s) = onClickOperation s
    onClickC :: CalcStep -> State CalcState ()
    onClickC (CalcStep s) = onClickC s
    onClickEqual :: CalcStep -> State CalcState ()
    onClickEqual (CalcStep s) = onClickEqual s

calc :: Dv.DispVal -> Dv.DispVal -> Maybe Operation -> Dv.DispVal
calc _ _ Nothing = zeroDispVal
calc dv1 dv2 (Just x)
    | x == Plus = dv1 + dv2
    | x == Minus = dv1 - dv2
    | x == Prod = dv1 * dv2
    | x == Div = dv1 / dv2
    | otherwise = zeroDispVal