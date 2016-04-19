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

{-# LANGUAGE Rank2Types, ConstraintKinds, FlexibleContexts #-}

module TypeNum.Integer.Positive
--(
--
----  PosInt(..),
--
--  PosInt'(..)
--, Positive, Positive'
--, positive
--
--, Positive2Int, Positive2Int'
--, Positive2Nat
--
--, pos2TInt, posIntValue
--
--)
where

import TypeNum
import TypeNum.Integer

import Data.Type.Bool

-----------------------------------------------------------------------------

data PosInt = One | PosSucc PosInt

data PosInt' (i :: PosInt) = PosInt'

-----------------------------------------------------------------------------

type family Positive2Nat (p :: PosInt) :: Nat

type family Positive2Int (p :: PosInt) :: TInt where
    Positive2Int One         = Succ Zero
    Positive2Int (PosSucc i) = Succ (Positive2Int i)

type family Positive2Int' i where
    Positive2Int' (PosInt' i) = Int' (Positive2Int i)


pos2TInt :: (Positive2Int i ~ j) => PosInt' i -> Int' j
pos2TInt = const Int'


type PosIntValue i = (TIntValue (Positive2Int i))

posIntValue :: (PosIntValue i) => PosInt' i -> Integer
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

instance (TIntValue (Positive2Int p)) => Show (PosInt' p) where
    show = show . posIntValue


instance TypesEq (a :: PosInt) (b :: PosInt) where
    type a == b  = Positive2Nat a == Positive2Nat b
instance TypesOrd (a :: PosInt) (b :: PosInt) where
    type Cmp a b = Cmp (Positive2Nat a) (Positive2Nat b)

instance TypesEq (a :: PosInt) (b :: Nat)  where
    type a == b  = Positive2Nat a == b
instance TypesOrd (a :: PosInt) (b :: Nat) where
    type Cmp a b = Cmp (Positive2Nat a) b

instance TypesEq (a :: PosInt) (b :: TInt) where
    type a == b  = Positive2Int a == b
instance TypesOrd (a :: PosInt) (b :: TInt) where
    type Cmp a b = Cmp (Positive2Int a) b


-----------------------------------------------------------------------------

instance (TIntValue (Positive2Int p)) =>
    TypeNumValue (p :: PosInt) where type NumValue (p :: PosInt) = Integer
                                     type NumContainer (p :: PosInt) = PosInt'
                                     runtimeValue = posIntValue

-----------------------------------------------------------------------------

instance TypesNat (a :: PosInt) (b :: PosInt) (c :: PosInt) where
    type a +  b = PositiveUnsafe (a  + b)
    type a /- b = PositiveUnsafe (a /- b)
    type a *  b = PositiveUnsafe (a  * b)


instance TypesNat (a :: PosInt) (b :: PosInt) (c :: Nat) where
    type a +  b = (Positive2Nat a +  Positive2Nat b :: Nat)
    type a /- b = (Positive2Nat a /- Positive2Nat b :: Nat)
    type a *  b = (Positive2Nat a *  Positive2Nat b :: Nat)

instance TypesNat (a :: PosInt) (b :: Nat) (c :: Nat) where
    type a +  b = (Positive2Nat a +  b :: Nat)
    type a /- b = (Positive2Nat a /- b :: Nat)
    type a *  b = (Positive2Nat a *  b :: Nat)



instance TypesNat (a :: PosInt) (b :: PosInt) (c :: TInt) where
    type a +  b = Positive2Int (a  + b)
    type a /- b = Positive2Int (a /- b)
    type a *  b = Positive2Int (a  * b)

instance TypesNat (a :: PosInt) (b :: Nat) (c :: TInt) where
    type a +  b = Positive2Int (a  + b)
    type a /- b = Positive2Int (a /- b)
    type a *  b = Positive2Int (a  * b)

-----------------------------------------------------------------------------

instance TypesIntegral (a :: PosInt) (b :: PosInt) (c :: Nat) where
    type QuotRem a b = (QuotRem (Positive2Nat a) (Positive2Nat b) :: (Nat, Nat))
    type DivMod  a b = (DivMod  (Positive2Nat a) (Positive2Nat b) :: (Nat, Nat))

instance TypesIntegral (a :: PosInt) (b :: PosInt) (c :: TInt) where
    type QuotRem a b = (QuotRem (Positive2Int a) (Positive2Int b) :: (TInt, TInt))
    type DivMod  a b = (DivMod  (Positive2Int a) (Positive2Int b) :: (TInt, TInt))


-----------------------------------------------------------------------------






