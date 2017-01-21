

module Main where

import TypeNum.Test.Common
import TypeNum.Test.Nat
import TypeNum.Test.Int
import TypeNum.Test.PosInt
import TypeNum.Test.Rational

-----------------------------------------------------------------------------


main = hspec $ do natSpec
                  intSpec
                  posIntSpec
                  rationalSpec
