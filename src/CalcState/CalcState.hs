module CalcState.CalcState(
    --CalcState,
    ICalcState,
    --OperationSelectedState,
    --SecondInputState
) where

import qualified DispValue as Dv
import Control.Monad.State.Lazy

--data CalcState = forall a. (ICalcState a) => CalcState a
--data OperationSelectedState = OperationSelectedState
--data SecondInputState = SecondInputState

class ICalcState a where
    addNumber :: a -> Int -> State Dv.DispVal ()
    addNumber _ num = do
        dv <- get
        put (Dv.addNumber dv num)
    addDot :: a -> State Dv.DispVal ()
    addDot _ = do
        dv <- get
        put (Dv.addDot dv)
