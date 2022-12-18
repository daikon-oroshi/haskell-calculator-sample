module Main where

import Gui
import Control.Monad.State
import DispValue (zeroDispVal)

import Control.Monad
import Control.Monad.ST
import Data.STRef

main :: IO ()
main = runGui