-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.TTest.PosInt
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module TypeNum.TTest.PosInt where

import TypeNum.TTest.Common
import TypeNum.Integer.Positive


-----------------------------------------------------------------------------

posIntSpec = describe "TypeNum.Integer.Positive.PosInt" $ do

    describe "is comparable at type-level (TypesEq and TypesOrd)" $ do
        specify "=="  $ correct(B::B( Nat2Positive 2 == PosSucc One ))
                     && mistake(B::B( Nat2Positive 2 == One ))

        specify ">"   $ correct (B::B( Nat2Positive 2 >  One ))

        specify "<"   $ correct (B::B( Nat2Positive 2 <   Nat2Positive 3 ))
        specify ">="  $ correct (B::B( Nat2Positive 3 >=  Nat2Positive 3 ))
        specify "<="  $ correct (B::B( Nat2Positive 2 <=  Nat2Positive 3 ))

    describe "has natural number operations at type-level (TypesNat)" $ do
        it "provides type-level sum '(+)'"
            $ correct (B::B( Nat2Positive 2 + Nat2Positive 1 == Nat2Positive 3 ))

        it "provides type-level multiplication '(*)'"
            $ correct (B::B( Nat2Positive 2 * Nat2Positive 3 == Nat2Positive 6 ))

        it "provides type-level power '(^)'" $ example pending

    describe "has integral number operations at type-level (TypesIntegral)" $ do
        it "provides type-level integer division truncated toward zero 'Quot'" $
            example pending
        it "provides type-level integer division truncated toward negative infinity 'Div'" $
            example pending



