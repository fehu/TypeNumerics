

module Main where

import TypeNum.TTest.Common
import TypeNum.TTest.Nat
import TypeNum.TTest.Int
import TypeNum.TTest.Rational

-----------------------------------------------------------------------------


main = hspec $ do natSpec
                  intSpec
                  rationalSpec

