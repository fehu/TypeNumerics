-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Integer
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

{-# LANGUAGE FlexibleInstances #-}

module TypeNum.Integer (

  TInt(..), Int'(..)
, Pos, Neg

, TIntValue(..)

, SuccOf, PrevOf
, iSucc, iPrev
, AbsNat

, module TypeNum.Nat
, TypeIntegral(..), TypeSign(..), TypeSubtraction(..)
, TypeNumValue(..)

) where

import TypeNum
import TypeNum.Nat

import Data.Type.Bool
import Data.Type.Equality hiding (type (==))
import Data.Type.Equality

--import GHC.TypeLits (ErrorMessage) TODO: update GHC

-----------------------------------------------------------------------------


data TInt = Zero
          | Succ TInt
          | Prev TInt

data (TIntValue i) => Int' (i :: TInt) = Int'

-----------------------------------------------------------------------------

type family SuccOf (i :: TInt) :: TInt where
    SuccOf (Prev i) = i
    SuccOf i        = Succ i

type family PrevOf (i :: TInt) :: TInt where
    PrevOf (Succ i) = i
    PrevOf i        = Prev i


iSucc :: Int' i -> Int' (SuccOf i)
iSucc = const Int'

iPrev :: Int' i -> Int' (PrevOf i)
iPrev = const Int'


-----------------------------------------------------------------------------

class TIntValue (i :: TInt) where intValue :: Int' i -> Integer

instance TIntValue Zero where intValue _ = 0
instance (TIntValue i) => TIntValue (Succ i) where intValue i = intValue (iPrev i) + 1
instance (TIntValue i) => TIntValue (Prev i) where intValue i = intValue (iSucc i) - 1


instance (TIntValue i) => Show (Int' i) where show = show . intValue

-----------------------------------------------------------------------------

--instance

type family Pos (n :: Nat) :: TInt where
    Pos 0 = Zero
    Pos i = Succ (Pos (i /- 1))

type family Neg (n :: Nat) :: TInt where
    Neg 0 = Zero
    Neg i = Prev (Neg (i /- 1))

posInt :: Nat' i -> Int' (Pos i)
posInt = const Int'

negInt :: Nat' i -> Int' (Neg i)
negInt = const Int'

-----------------------------------------------------------------------------


type family IntEq (a :: TInt) (b :: TInt) :: Bool where
    IntEq Zero     Zero     = True
    IntEq (Succ a) (Succ b) = IntEq a b
    IntEq (Prev a) (Prev b) = IntEq a b
    IntEq a        b        = False

type instance a == b = IntEq a b

type family AbsNat (i :: TInt) :: Nat where
    AbsNat Zero     = 0
    AbsNat (Succ a) = 1 + AbsNat a
    AbsNat (Prev a) = 1 + AbsNat a

type family CmpInt (a :: TInt) (b :: TInt) :: Ordering where
    CmpInt Zero     Zero     = EQ
    CmpInt (Succ a) (Prev b) = GT
    CmpInt (Prev a) (Succ b) = LT

    CmpInt (Succ a) Zero     = GT
    CmpInt Zero     (Succ b) = LT
    CmpInt (Succ a) (Succ b) = CmpInt a b

    CmpInt (Prev a) Zero     = LT
    CmpInt Zero     (Prev b) = GT
    CmpInt (Prev a) (Prev b) = CmpInt a b


type family SignumInt (a :: TInt) :: Sign where
    SignumInt Zero     = SignZero
    SignumInt (Succ a) = SignPos
    SignumInt (Prev a) = SignNeg

type family SignInt (a :: Sign) :: TInt where
    SignInt SignZero = Zero
    SignInt SignPos  = Succ Zero
    SignInt SignNeg  = Prev Zero



type family NegateInt (a :: TInt) :: TInt where
    NegateInt Zero = Zero
    NegateInt (Succ x) = Prev (Negate x)
    NegateInt (Prev x) = Succ (Negate x)


type family IntPlus (a :: TInt) (b :: TInt) :: TInt where
    IntPlus a Zero = a
    IntPlus a (Succ b') = SuccOf (IntPlus a b')
    IntPlus a (Prev b') = PrevOf (IntPlus a b')

type family IntDiff (a :: TInt) (b :: TInt) :: TInt where
    IntDiff Zero     Zero       = Zero
    IntDiff (Succ a) (Prev b)   = Succ ( Succ (IntDiff a b) )
    IntDiff (Prev a) (Succ b)   = Succ ( Succ (IntDiff a b) )

    IntDiff (Succ a) Zero       = Succ (IntDiff a Zero)
    IntDiff Zero     (Succ b)   = Succ (IntDiff Zero b)
    IntDiff (Succ a) (Succ b)   = IntDiff a b

    IntDiff (Prev a) Zero       = Succ (IntDiff a Zero)
    IntDiff Zero     (Prev b)   = Succ (IntDiff Zero b)
    IntDiff (Prev a) (Prev b)   = IntDiff a b

type family IntMult' (a :: TInt) (b :: TInt) :: Nat where
    IntMult' a b = AbsNat a * AbsNat b

--type family IntMult (a :: TInt) (b :: TInt) :: TInt where
--    IntMult a    Zero = Zero
--    IntMult Zero b    = Zero
--
--    IntMult (Succ a) (Succ b) = IntPlus (Succ b) (IntMult a (Succ b))
--    IntMult (Succ a) (Prev b) = IntPlus (Prev b) (IntMult a (Prev b))
--    IntMult (Prev a) (Prev b) = IntPlus (Succ b) (IntMult a (Prev b))


type family QuotRemInt (a :: TInt) (b :: TInt) :: (TInt, TInt) where
    QuotRemInt a b = QuotRemSign (SignsMult (Signum a) (Signum b))
                                 (QuotRemInt' (Abs a) (CmpInt (Abs a) (Abs b)) (Abs b) Zero)


type family QuotRemSign (sign :: Sign) (quotrem :: (TInt, TInt)) :: (TInt, TInt) where
    QuotRemSign SignNeg '(q,r) = '(Negate q, Negate r)
    QuotRemSign sign    '(q,r) = '(q,r)

--
type family QuotRemInt' (a :: TInt) (ord :: Ordering) (b :: TInt) (quot :: TInt) :: (TInt, TInt)
    where QuotRemInt' a LT b quot = '(quot, a)
          QuotRemInt' a EQ b quot = '(Succ quot, Zero)
          QuotRemInt' a GT b quot = QuotRemInt' (IntDiff a b) (CmpInt (IntDiff a b) b) b (Succ quot)

-----------------------------------------------------------------------------

instance TypesEq  (a :: TInt) (b :: TInt) where type a ~=~ b = IntEq a b
instance TypesOrd (a :: TInt) (b :: TInt) where type Cmp a b = CmpInt a b

instance TypesEq  (a :: TInt) (b :: Nat)  where type a ~=~ b = a == Pos b
instance TypesOrd (a :: TInt) (b :: Nat)  where type Cmp a b = CmpInt a (Pos b)

instance TypesEq  (a :: Nat) (b :: TInt)  where type a ~=~ b = Pos a == b
instance TypesOrd (a :: Nat) (b :: TInt)  where type Cmp a b = CmpInt (Pos a) b

type instance a == b = IntEq a b

-----------------------------------------------------------------------------

instance (TIntValue a) =>
    TypeNumValue (a :: TInt) where type NumValue (a :: TInt) = Integer
                                   type NumContainer (a :: TInt) = Int'
                                   runtimeValue = intValue

-----------------------------------------------------------------------------

instance TypeSign (a :: TInt) where type Signum a   = SignumInt a
                                    type Abs a      = Pos (AbsNat a)
                                    type Negate a   = NegateInt a
                                    type FromSign s = SignInt s

-----------------------------------------------------------------------------

type family WithSign (a :: TInt) (b :: TInt) (n :: Nat) :: TInt where
    WithSign a b n = If (SignsMult (Signum a) (Signum b) == SignNeg) (Neg n) (Pos n)

type family WithSign' (a :: TInt) (b :: TInt) (n :: (Nat, Nat)) :: (TInt, TInt) where
    WithSign' a b '(x,y) = If (SignsMult (Signum a) (Signum b) == SignNeg) '(Neg x, Neg y)
                                                                           '(Pos x, Pos y)


instance TypeNat (a :: TInt) b where
    type a +  b = IntPlus a b
    type a *  b = WithSign a b (IntMult' a b)

instance TypeAbsDiff (a :: TInt) b where type a /- b = IntDiff a b

instance TypeIntegral (a :: TInt) b where
    type QuotRem a b = QuotRemInt a b -- WithSign' a b (QuotRem (AbsNat a) (AbsNat b))
    type DivMod  a b = WithSign' a b (DivMod  (AbsNat a) (AbsNat b))

-----------------------------------------------------------------------------



