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
        specify "==" $ correct (B::B( Pos 4 :% 5 == Pos 4:%5 ))
                    && correct (B::B( Pos 4:%4 == AsRational (Pos 1) ))
                    && correct (B::B( Pos 4:%4 ~~ Pos 1:%1 ))
                    && correct (B::B( Pos 4:%4 ~~ Pos 1 ))
                    && correct (B::B( Pos 2:%4 ~~ Pos 1:%2 ))


        specify ">"  $ correct (B::B( (Pos 7:%5) > (Pos 4:%5) ))
                    && correct (B::B( (Pos 1:%2) > (Pos 1:%3) ))
                    && mistake (B::B( (Pos 1:%3) > (Pos 1:%2) ))
                    && correct (B::B( (Pos 4:%5) > (Neg 7:%5) ))

        specify "<"  $ correct (B::B( (Pos 11:%15)       < (Pos 13:%15) ))
                    && correct (B::B( AsRational (Pos 1) < (Pos 15:%13) ))
                    && mistake (B::B( (Pos 1:%5)         < (Pos 1:%6) ))
                    && correct (B::B( (Neg 100:%3)       < AsRational Zero ))

        specify ">=" $ example pending
        specify "<=" $ example pending

    describe "has natural number operations at type-level (TypesNat)" $ do

        it "provides type-level sum '(+)'"
                $ correct (B::B(  (Pos 2:%2) + (Pos 2:%2) ~~ Pos 2    ))
               && correct (B::B(  (Pos 2:%2) + (Pos 2:%2) == Pos 2:%1 ))
               && correct (B::B(  (Pos 3:%2) + (Pos 3:%2) == Pos 3:%1 ))
               && correct (B::B(  (Pos 1:%2) + (Pos 1:%2) ~~ Pos 1    ))
               && correct (B::B(  (Pos 1:%3) + (Pos 1:%2) == Pos 5:%6 ))
               && correct (B::B(  (Pos 3:%7) + (Pos 4:%7) == Pos 7:%7 ))
               && correct (B::B(  (Pos 3:%7) + (Pos 4:%7) ~~ Pos 1    ))
               && correct (B::B(  (Pos 1:%3) + (Pos 1:%2) == Pos 5:%6 ))
               && correct (B::B(  (Neg 1:%3) + (Neg 1:%2) == Neg 5:%6 ))
               && correct (B::B(  (Pos 1:%3) + (Neg 1:%2) == Neg 1:%6 ))

        it "provides type-level absolute difference '(/-)'"
                $ correct (B::B( (Pos 1:%1)     /- (Pos 1:%1)     ~~ Zero     ))
               && correct (B::B( (AsRational 1) /- (AsRational 1) ~~ Zero     ))
               && correct (B::B( (Pos 3:%2)     /- (Pos 1:%2)     ~~ Pos 1    ))
               && correct (B::B( (AsRational 1) /- (Pos 1:%2)     == Pos 1:%2 ))
               && correct (B::B( (Neg 3:%2)     /- (Pos 1:%2)     ~~ Pos 2    ))
               && correct (B::B( (Pos 1:%2)     /- (Neg 3:%2)     ~~ Pos 2    ))


        it "provides type-level multiplication '(*)'"
                $ correct (B::B( (Pos 1:%1) * (Pos 1:%1)      ~~ 1        ))
               && correct (B::B( (Pos 1:%2) * (Pos 1:%3)      == Pos 1:%6 ))
               && correct (B::B( (Pos 1:%2) * (AsRational 2)  ~~ 1        ))
               && correct (B::B( (Pos 2:%3) * (Pos 3:%5)      == Pos 2:%5 ))
               && correct (B::B( (Pos 2:%3) * (Neg 1:%3)      == Neg 2:%9 ))

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

