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


module TypeNum.Integer where


import GHC.TypeLits
import Data.Type.Equality

-----------------------------------------------------------------------------


data TInt = Zero
          | Succ TInt
          | Prev TInt

data Int' (i :: TInt) = Int'

-----------------------------------------------------------------------------

type instance (==) Zero Zero = True
type instance (==) (Succ a) (Succ b) = a == b
type instance (==) (Prev a) (Prev b) = a == b

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

type family Pos (n :: Nat) :: TInt where
    Pos 0 = Zero
    Pos i = Succ (Pos (i-1))

type family Neg (n :: Nat) :: TInt where
    Neg 0 = Zero
    Neg i = Prev (Neg (i-1))

posInt :: Nat' i -> Int' (Pos i)
posInt = const Int'

negInt :: Nat' i -> Int' (Neg i)
negInt = const Int'

-----------------------------------------------------------------------------

type family (+) (a :: TInt) (b :: TInt) :: TInt where
    Zero      TypeNum.Integer.+ b = b
    (Succ a') TypeNum.Integer.+ b = SuccOf (a' TypeNum.Integer.+ b)
    (Prev a') TypeNum.Integer.+ b = PrevOf (a' TypeNum.Integer.+ b)


type family Negate (a :: TInt) :: TInt where
    Negate Zero = Zero
    Negate (Succ x) = Prev (Negate x)
    Negate (Prev x) = Succ (Negate x)

-----------------------------------------------------------------------------

data Nat' (n :: Nat) = Nat'

