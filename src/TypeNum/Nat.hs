-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Nat
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module TypeNum.Nat (

  Nat
, Nat'(Nat')

, NatSucc, NatPrev

, TypesEq(..), type (=~=), type  (/~=), type (==), TypesOrd(..)

, TypeNat(..)


) where

import TypeNum

import GHC.TypeLits (Nat, CmpNat)
import qualified GHC.TypeLits as TL

import Data.Type.Bool
import Data.Type.Equality (type (==))


-----------------------------------------------------------------------------

data Nat' (n :: Nat) = Nat'

instance TypesEq  (a :: Nat) (b :: Nat) where type a ~=~ b  = a == b
instance TypesOrd (a :: Nat) (b :: Nat) where type Cmp a b = CmpNat a b


instance (TL.KnownNat a) =>
    TypeNumValue (a :: Nat) where type NumValue (a :: Nat) = Integer
                                  type NumContainer (a :: Nat) = Nat'
                                  runtimeValue = TL.natVal

instance TypeNat (a :: Nat) b where
    type a +  b = a TL.+ b
    type a *  b = a TL.* b
    type a ^  b = a TL.^ b

instance TypeAbsDiff (a :: Nat) (b :: Nat) where
    type a /- b = If (a > b) (a TL.- b) (b TL.- a)

--instance TypesIntegral (a :: Nat) (b :: Nat) (c :: Nat) where
--    type QuotRem a b = QuotRemNat' a (Cmp a b) b 0
--    type DivMod  a b = QuotRemNat' a (Cmp a b) b 0


-----------------------------------------------------------------------------

type family NatSucc (n :: Nat) :: Nat where
    NatSucc 0 = 1
    NatSucc x = x + 1

type family NatPrev (n :: Nat) :: Maybe Nat where
    NatPrev 0 = Nothing
    NatPrev n = Just (n - 1)

-----------------------------------------------------------------------------

--type family QuotRemNat (a :: Nat) (b :: Nat) (quot :: Nat) :: (Nat, Nat) where
--    QuotRemNat a b quot = If (a < b) '(quot, a) (QuotRemNat (a - b) b (quot + 1))


type family QuotRemNat' (a :: Nat) (ord :: Ordering) (b :: Nat) (quot :: Nat) :: (Nat, Nat) where
    QuotRemNat' a LT b quot = '(quot, a)
    QuotRemNat' a EQ b quot = '(quot + 1, 0)
    QuotRemNat' a GT b quot = QuotRemNat' (a - b) (Cmp (a - b) b) b (quot + 1)
