module CalcState() where
import Control.Monad.State
import qualified DispValue as Dv

addNumber :: Int -> State Dv.DispVal ()
addNumber a = do
    val <- get
    put $ Dv.addNumber val a
