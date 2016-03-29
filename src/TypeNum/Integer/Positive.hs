-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Integer.Positive
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE Rank2Types #-}

module TypeNum.Integer.Positive where

import TypeNum.Integer

import GHC.TypeLits
import Data.Type.Equality

-----------------------------------------------------------------------------

data PosInt = One | PosSucc PosInt

data PosInt' (i :: PosInt) = PosInt'

-----------------------------------------------------------------------------

type family Positive2Int (p :: PosInt) :: TInt where
    Positive2Int One         = Succ Zero
    Positive2Int (PosSucc i) = Succ (Positive2Int i)

type family Positive2Int' i where
    Positive2Int' (PosInt' i) = Int' (Positive2Int i)


pos2TInt :: (Positive2Int i ~ j) => PosInt' i -> Int' j
pos2TInt = const Int'

posIntValue :: (TIntValue n, Positive2Int i ~ n) => PosInt' i -> Integer
posIntValue = intValue . pos2TInt


type family PositiveUnsafe (n :: Nat) :: PosInt where
    PositiveUnsafe 1 = One
    PositiveUnsafe i = PosSucc (PositiveUnsafe (i-1))


type family Positive (n :: Nat) :: Maybe PosInt where
    Positive 0 = Nothing
    Positive i = Just (PositiveUnsafe i)

type Positive' (n :: Nat) = ((n == 0) ~ False) => PosInt' (PositiveUnsafe n)

positive :: Nat' n -> Positive' n
positive _ = PosInt'

-----------------------------------------------------------------------------

instance (TIntValue n, Positive2Int i ~ n) => Show (PosInt' i) where show = show . posIntValue


