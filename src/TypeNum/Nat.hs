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

, TypesEq(..), TypesOrd(..), TypesNat(..)

) where

import TypeNum

import GHC.TypeLits (Nat, CmpNat)
import qualified GHC.TypeLits as TL

import Data.Type.Bool
import qualified Data.Type.Equality as Eq


-----------------------------------------------------------------------------

data Nat' (n :: Nat) = Nat'

instance TypesEq  (a :: Nat) (b :: Nat) where type a == b  = a Eq.== b
instance TypesOrd (a :: Nat) (b :: Nat) where type Cmp a b = CmpNat a b


instance (TL.KnownNat a) =>
    TypeNumValue (a :: Nat) where type NumValue (a :: Nat) = Integer
                                  type NumContainer (a :: Nat) = Nat'
                                  runtimeValue = TL.natVal

instance TypesNat (a :: Nat) (b :: Nat) (c :: Nat) where
    type a +  b = a TL.+ b
    type a /- b = If (a > b) (a TL.- b) (b TL.- a)
    type a *  b = a TL.* b
    type a ^  b = a TL.^ b

--instance TypesIntegral (a :: Nat) (b :: Nat) (c :: Nat) where
--    type QuotRem a b = QuotRemNat' a (Cmp a b) b 0
--    type DivMod  a b = QuotRemNat' a (Cmp a b) b 0


-----------------------------------------------------------------------------

--type family QuotRemNat (a :: Nat) (b :: Nat) (quot :: Nat) :: (Nat, Nat) where
--    QuotRemNat a b quot = If (a < b) '(quot, a) (QuotRemNat (a - b) b (quot + 1))


type family QuotRemNat' (a :: Nat) (ord :: Ordering) (b :: Nat) (quot :: Nat) :: (Nat, Nat) where
    QuotRemNat' a LT b quot = '(quot, a)
    QuotRemNat' a EQ b quot = '(quot + 1, 0)
    QuotRemNat' a GT b quot = QuotRemNat' (a - b) (Cmp (a - b) b) b (quot + 1)





