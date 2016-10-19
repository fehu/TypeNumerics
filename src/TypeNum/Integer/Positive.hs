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
(

  PosInt(..), PosInt'(..)
, PosIntValue(..)

, PosIntConvert(..)
, Nat2PosInt(..)
, Int2PosInt(..)
)
where

import TypeNum
import TypeNum.Integer

import Data.Type.Bool
import Data.Type.Equality

-----------------------------------------------------------------------------

data PosInt = One | PosSucc PosInt

data PosInt' (i :: PosInt) = PosInt'

-----------------------------------------------------------------------------

class PosIntValue (i :: PosInt) where posIntValue :: PosInt' i -> Integer

instance PosIntValue One where posIntValue _ = 1
instance (PosIntValue i) => PosIntValue (PosSucc i) where posIntValue i = posIntValue (iPrev' i) + 1


type family PrevOf' i where PrevOf' (PosSucc i) = i

iPrev' :: PosInt' i -> PosInt' (PrevOf' i)
iPrev' = const PosInt'

-----------------------------------------------------------------------------

class PosIntConvert (p :: PosInt) where
    type Positive2Nat p :: Nat
    type Positive2Int p :: TInt

instance PosIntConvert One where
    type Positive2Nat One = 1
    type Positive2Int One = Succ Zero

instance PosIntConvert (PosSucc p) where
    type Positive2Nat (PosSucc p) = 1 + Positive2Nat p
    type Positive2Int (PosSucc p) = Succ (Positive2Int p)


class Nat2PosInt (n :: Nat)  where type Nat2Positive n :: PosInt
class Int2PosInt (i :: TInt) where type Int2Positive i :: PosInt

type family Nat2PosInt' (n :: Nat) :: PosInt where
    Nat2PosInt' 1 = One
    Nat2PosInt' n = PosSucc (Nat2PosInt' (n /- 1))

type family Int2PosInt' (i :: TInt) :: PosInt where
    Int2PosInt' (Succ Zero) = One
    Int2PosInt' (Succ i) = PosSucc (Int2PosInt' i)

instance ((n >= 1) ~ True) => Nat2PosInt n where
    type Nat2Positive n = Nat2PosInt' n

instance ((i >= 0) ~ True) => Int2PosInt i where
    type Int2Positive n = Int2PosInt' n


-----------------------------------------------------------------------------

instance (PosIntValue p) => Show (PosInt' p) where
    show = show . posIntValue


instance TypesEq (a :: PosInt) (b :: PosInt) where
    type a ~~ b  = Positive2Nat a == Positive2Nat b
instance TypesOrd (a :: PosInt) (b :: PosInt) where
    type Cmp a b = Cmp (Positive2Nat a) (Positive2Nat b)

type instance (a :: PosInt) == b = a ~~ b

instance TypesEq (a :: PosInt) (b :: Nat)  where
    type a ~~ b  = Positive2Nat a == b
instance TypesOrd (a :: PosInt) (b :: Nat) where
    type Cmp a b = Cmp (Positive2Nat a) b

instance TypesEq (a :: PosInt) (b :: TInt) where
    type a ~~ b  = Positive2Int a == b
instance TypesOrd (a :: PosInt) (b :: TInt) where
    type Cmp a b = Cmp (Positive2Int a) b

-----------------------------------------------------------------------------

instance (PosIntValue p) =>
    TypeNumValue (p :: PosInt) where type NumValue (p :: PosInt) = Integer
                                     type NumContainer (p :: PosInt) = PosInt'
                                     runtimeValue = posIntValue

-----------------------------------------------------------------------------

instance TypeNat (a :: PosInt) b where
    type a +  b = PosPlus a b
--    type a /- b = Nat2Positive (a /- b)
--    type a *  b = Nat2Positive (a  * b)


type family PosPlus a b where
    PosPlus One x = PosSucc x
    PosPlus x One = PosSucc x
    PosPlus (PosSucc y) x = PosPlus y (PosSucc x)

--type family PosDiff a b where
--    PosDiff

-----------------------------------------------------------------------------

--instance TypeIntegral (a :: PosInt) b where
--    type QuotRem a b = (QuotRem (Positive2Nat a) (Positive2Nat b) :: (PosInt, PosInt))
--    type DivMod  a b = (DivMod  (Positive2Nat a) (Positive2Nat b) :: (PosInt, PosInt))

-----------------------------------------------------------------------------


--instance Convertible (x :: PosInt) (y :: TInt) where type Convert x = Positive2Int x



