module DispValueSpec (spec) where

import Test.Hspec
import DispValue ( unitDispVal, zeroDispVal )

spec :: Spec
spec = do
    describe "addDispValue" $
        it "0 と 1の足し算" $
            (zeroDispVal + unitDispVal) `shouldBe` unitDispVal