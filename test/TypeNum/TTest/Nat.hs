-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.TTest.Nat
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

--{-# LANGUAGE ConstraintKinds #-}

module TypeNum.TTest.Nat where

import TypeNum.TTest.Common
import TypeNum.Nat

-----------------------------------------------------------------------------

natSpec = describe "GHC.TypeLits.Nat" $ do

    describe "is comparable at type-level" $ do

        specify "==" $ correct (B::B (( 1 :: Nat) == ( 1 :: Nat)))
                    && correct (B::B ((17 :: Nat) == (17 :: Nat)))
                    && mistake (B::B (( 1 :: Nat) == (17 :: Nat)))

        specify ">"  $ correct (B::B ((2 :: Nat) > (1 :: Nat)))
                    && mistake (B::B ((0 :: Nat) > (1 :: Nat)))

        specify "<"  $ correct (B::B ((1 :: Nat) < (2 :: Nat)))
                    && mistake (B::B ((5 :: Nat) < (0 :: Nat)))

        specify ">=" $ correct (B::B ((2 :: Nat) >= (1 :: Nat)))
                    && correct (B::B ((1 :: Nat) >= (1 :: Nat)))
                    && mistake (B::B ((0 :: Nat) >= (1 :: Nat)))

        specify "<=" $ correct (B::B ((1 :: Nat) <= (2 :: Nat)))
                    && correct (B::B ((2 :: Nat) <= (2 :: Nat)))
                    && mistake (B::B ((5 :: Nat) <= (0 :: Nat)))

    describe "has natural number operations at type-level" $ do

        it "provides type-level sum '(+)'" $
               correct (B::B ((1 + 2 :: Nat) == (2 + 1 :: Nat)))
            && correct (B::B ((1 + 2 :: Nat) == (3 :: Nat)))
            && mistake (B::B ((1 + 2 :: Nat) == (2 :: Nat)))

        it "provides type-level absolute difference '(/-)'" $
               correct (B::B ((1 /- 3 :: Nat) == (2 :: Nat)))
            && correct (B::B ((3 /- 1 :: Nat) == (2 :: Nat)))
            && correct (B::B ((1 /- 1 :: Nat) == (0 :: Nat)))
            && mistake (B::B ((3 /- 3 :: Nat) == (2 :: Nat)))

        it "provides type-level multiplication '(*)'" $
               correct (B::B ((1 * 3 :: Nat) == (3 :: Nat)))
            && correct (B::B ((0 * 3 :: Nat) == (0 :: Nat)))
            && correct (B::B ((3 * 3 :: Nat) == (9 :: Nat)))
            && mistake (B::B ((2 * 3 :: Nat) == (8 :: Nat)))

    describe "has integral number operations at type-level" $ do

        it "provides type-level integer division truncated toward zero 'Quot'" $ pending

--               correct (B::B (((QuotRem 3 3) :: (Nat, Nat)) == '(1, 0)))
--            && correct (B::B (((QuotRem 1 2) :: (Nat, Nat)) == '(0, 1)))
--            && correct (B::B (((QuotRem 2 1) :: (Nat, Nat)) == '(2, 0)))
--            && correct (B::B (((Quot 4 3) :: Nat) == (1 :: Nat)))





