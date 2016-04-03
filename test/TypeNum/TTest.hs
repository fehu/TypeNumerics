

module Main where

import TypeNum.TTest.Common
import TypeNum.TTest.Nat
import TypeNum.TTest.Int

-----------------------------------------------------------------------------


main = hspec $ do natSpec
                  intSpec

