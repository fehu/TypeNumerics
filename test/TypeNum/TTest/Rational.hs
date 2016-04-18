-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.TTest.Rational
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

module TypeNum.TTest.Rational where

import TypeNum.TTest.Common
import TypeNum.Rational

-----------------------------------------------------------------------------

rationalSpec = describe "TypeNum.Integer.Rational'" $ do

    describe "is comparable at type-level (TypesEq and TypesOrd)" $ do
        specify "==" $ example pending
        specify ">"  $ example pending
        specify "<"  $ example pending
        specify ">=" $ example pending
        specify "<=" $ example pending

    describe "has natural number operations at type-level (TypesNat)" $ do
        it "provides type-level sum '(+)'"                  $ example pending
        it "provides type-level absolute difference '(/-)'" $ example pending
        it "provides type-level multiplication '(*)'"       $ example pending
        it "provides type-level power '(^)'"                $ example pending

    describe "has sign operations at type-level (TypeSign)" $ do
        it "provides type-level sign"           $ example pending
        it "provides type-level absolute value" $ example pending
        it "provides type-level unary negation" $ example pending
        it "provides type-level sign to number transformation" $ example pending

    describe "has subtraction operation at type-level (TypesSubtraction)" $
        it "provides type-level subtraction (-)" $ example pending

    describe "has rational number operations at type-level (TypesRational)" $
        it "provides type-level rational division (/)" $ example pending




