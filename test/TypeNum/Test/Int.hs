-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Test.Int
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module TypeNum.Test.Int where

import TypeNum.Test.Common
import TypeNum.Integer

-----------------------------------------------------------------------------

intSpec = describe "TypeNum.Integer.TInt" $ do

    describe "is comparable at type-level ((==), TypesEq and TypesOrd)" $ do

        specify "==" $ correct (B::B (Pos 1 == Pos 1))
                    && correct (B::B (Pos 1 ~=~ 1))
                    && correct (B::B (Neg 5 == Neg 5))

                    && correct (B::B (Pos 0 == Neg 0))
                    && correct (B::B (Pos 0 ~=~ 0))

                    && mistake (B::B (Neg 1 ~=~ 1))
                    && mistake (B::B (Pos 5 == Pos 2))

        specify ">"  $ correct (B::B (Pos 2 > Pos 1))
                    && correct (B::B (Neg 1 > Neg 2))
                    && correct (B::B (Pos 0 > Neg 2))

                    && mistake (B::B (Pos 2 > Pos 5))
                    && mistake (B::B (Pos 0 > Pos 1))
                    && mistake (B::B (Neg 1 > Pos 1))


        specify "<"  $ correct (B::B (Pos 1 < Pos 2))
                    && correct (B::B (Neg 1 < Pos 2))
                    && correct (B::B (Neg 2 < Neg 1))

                    && mistake (B::B (Neg 4 < Neg 4))
                    && mistake (B::B (Pos 4 < Neg 1))

        specify ">=" $ correct (B::B (Pos 2 >= Pos 1))
                    && correct (B::B (Neg 1 >= Neg 2))
                    && correct (B::B (Pos 0 >= Neg 2))

                    && correct (B::B (Pos 2 >= Pos 2))

                    && mistake (B::B (Pos 0 >= Pos 1))
                    && mistake (B::B (Neg 1 >= Pos 1))
--
        specify "<=" $ correct (B::B (Pos 1 <= Pos 2))
                    && correct (B::B (Neg 1 <= Pos 2))
                    && correct (B::B (Neg 2 <= Neg 1))

                    && correct (B::B (Neg 4 <= Neg 4))

                    && mistake (B::B (Pos 4 <= Neg 1))

    describe "has natural number operations at type-level (TypesNat)" $ do

        it "provides type-level sum '(+)'" $
               correct (B::B ((Pos 1 + Pos 1 :: TInt) == Pos 2))
            && correct (B::B ((Pos 4 + Pos 5 :: TInt) == Pos 9))
            && correct (B::B ((Neg 4 + Pos 5 :: TInt) == Pos 1))
            && correct (B::B ((Neg 5 + Pos 5 :: TInt) == Pos 0))

            && correct (B::B ((Pos 4 + Pos 5 :: TInt) == (Pos 2 + Pos 7 :: TInt)))
            && correct (B::B ((Neg 4 + Pos 5 :: TInt) == (Pos 5 + Neg 4 :: TInt)))
            && correct (B::B ((Neg 5 + Pos 5 :: TInt) == (Pos 4 + Neg 4 :: TInt)))

            && mistake (B::B ((Neg 4 + Pos 5 :: TInt) == Pos 4))
            && mistake (B::B ((Neg 5 + Pos 5 :: TInt) == Neg 1))

            && mistake (B::B ((Pos 4 + Pos 5 :: TInt) == (Pos 2 + Pos 2 :: TInt)))
            && mistake (B::B ((Neg 4 + Pos 5 :: TInt) == (Pos 5 + Pos 4 :: TInt)))

        it "provides type-level absolute difference '(/-)'" $
               correct (B::B ((Pos 1 /- Pos 3 :: TInt) ~=~ 2))
            && correct (B::B ((Pos 3 /- Pos 1 :: TInt) ~=~ 2))
            && correct (B::B ((Pos 3 /- Pos 3 :: TInt) ~=~ 0))

            && correct (B::B ((Neg 1 /- Neg 3 :: TInt) ~=~ 2))
            && correct (B::B ((Neg 1 /- Pos 3 :: TInt) ~=~ 4))
            && correct (B::B ((Pos 1 /- Neg 3 :: TInt) ~=~ 4))

            && mistake (B::B ((Pos 3 /- Pos 3 :: TInt) ~=~ 2))

        it "provides type-level multiplication '(*)'" $
               correct (B::B ((Pos 1 * Pos 3 :: TInt) ~=~ 3))
            && correct (B::B ((Pos 3 * Pos 1 :: TInt) ~=~ 3))

            && correct (B::B ((Neg 3 * Pos 1 :: TInt) == Neg 3))
            && correct (B::B ((Neg 3 * Neg 1 :: TInt) == Pos 3))

            && correct (B::B ((Neg 3 * Zero :: TInt) ~=~ 0))
            && correct (B::B ((Zero * Pos 9 :: TInt) ~=~ 0))

            && mistake (B::B ((Pos 2 * Pos 3 :: TInt) ~=~ 9))

        it "provides type-level power '(^)'" $ pending

    describe "has integral number operations at type-level (TypesIntegral)" $ do

        it "provides type-level integer division truncated toward zero 'Quot'" $

               correct (B::B (((QuotRem (Pos 3) (Pos 3)) :: (TInt, TInt)) == ('(Pos 1, Zero))))
            && correct (B::B (((QuotRem (Pos 1) (Pos 2)) :: (TInt, TInt)) == ('(Zero, Pos 1))))
            && correct (B::B (((QuotRem (Pos 2) (Pos 1)) :: (TInt, TInt)) == ('(Pos 2, Zero))))
            && correct (B::B (((QuotRem (Pos 4) (Pos 3)) :: (TInt, TInt)) == ('(Pos 1, Pos 1))))

            && correct (B::B (((QuotRem (Neg 4) (Pos 3)) :: (TInt, TInt)) == ('(Neg 1, Neg 1))))

            && correct (B::B (((Rem (Pos 3) (Pos 3)) :: TInt) == Zero))
            && correct (B::B (((Rem (Pos 3) (Pos 3)) :: TInt) < Pos 1))
            && correct (B::B (((Rem (Pos 4) (Pos 3)) :: TInt) == Pos 1))
            && correct (B::B (Zero < Pos 1))
            && correct (B::B ( ((Rem (Pos 3) (Pos 3)) :: TInt) < ((Rem (Pos 4) (Pos 3)) :: TInt) ))

        it "provides type-level integer division truncated toward negative infinity 'Div'" $
            example pending

    describe "has sign operations at type-level (TypeSign)" $ do
        it "provides type-level sign"           $ example pending
        it "provides type-level absolute value" $ example pending
        it "provides type-level unary negation" $ example pending
        it "provides type-level sign to number transformation" $ example pending

    describe "has subtraction operation at type-level (TypesSubtraction)" $
        it "provides type-level subtraction (-)" $ example pending
