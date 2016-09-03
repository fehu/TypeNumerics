-----------------------------------------------------------------------------
--
-- Module      :  TypeNum
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE PolyKinds, ConstraintKinds #-}

module TypeNum (

  Sign(..), SignsMult, SameSign

, TypeNumValue(..)
, TypesNat(..)
, TypesIntegral(..)
, TypeSign(..)
, TypesSubtraction(..)
, TypesRational(..)

, module TypeNum.TypeFunctions

) where

import Data.Type.Bool
import Data.Type.Equality

import TypeNum.TypeFunctions

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data Sign = SignPos | SignNeg | SignZero

type family SignsMult (a :: Sign) (b :: Sign) :: Sign where
    SignsMult SignPos  SignPos  = SignPos
    SignsMult SignNeg  SignNeg  = SignPos
    SignsMult a        SignZero = SignZero
    SignsMult SignZero b        = SignZero
    SignsMult a        b        = SignNeg

type family SameSign (a :: Sign) (b :: Sign) :: Bool where
    SameSign SignPos  SignPos  = True
    SameSign SignNeg  SignNeg  = True
    SameSign SignZero SignZero = True
    SameSign a        b        = False


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


class TypeNumValue (x :: a) where
    type NumValue x :: *
    type NumContainer x :: a -> *
    runtimeValue  :: (NumContainer x) x -> NumValue x


-----------------------------------------------------------------------------

-- | Natural-like numbers operations.
class ( TypesOrd x x, TypesOrd y y, TypesOrd z z
      , TypesOrd x y, TypesOrd x z, TypesOrd y z )
  =>
    TypesNat (x :: a) (y :: b) (z :: n) where
        type (+) (x :: a) (y :: b) :: n
        -- | Absolute difference.
        type (/-) (x :: a) (y :: b) :: n
        type (*) (x :: a) (y :: b) :: n
        type (^) (x :: a) (y :: b) :: n

-----------------------------------------------------------------------------

-- | Integral numbers, supporting integer division.
class (TypesNat x x x, TypesNat y y y, TypesNat z z z, TypesNat x y z) =>
    TypesIntegral (x :: a) (y :: b) (z :: n) where
        -- | Integer division truncated toward zero.
        type Quot (x :: a) (y :: b) :: n
        -- | Integer remainder.
        type Rem  (x :: a) (y :: b) :: n
        -- | Simultaneous 'Quot' and 'Rem'.
        type QuotRem (x :: a) (y :: b) :: (n, n)

        -- | Integer division truncated toward negative infinity.
        type Div  (x :: a) (y :: b) :: n
        -- | Integer modulus.
        type Mod  (x :: a) (y :: b) :: n
        -- | Simultaneous 'Div' and 'Mod'.
        type DivMod (x :: a) (y :: b) :: (n, n)

        type Quot a b = Fst (QuotRem a b)
        type Rem  a b = Snd (QuotRem a b)
        type Div  a b = Fst (DivMod a b)
        type Mod  a b = Snd (DivMod a b)

-----------------------------------------------------------------------------

-- | Signed numeric.
class TypeSign (num :: n)
    where
        -- | Sign of a number.
        type Signum (x :: n) :: Sign
        -- | Absolute value.
        type Abs    (x :: n) :: n
        -- | Unary negation.
        type Negate (x :: n) :: n
        -- | Corresponding 1, -1 or 0.
        type FromSign (s :: Sign) :: n

-----------------------------------------------------------------------------

class (TypeSign x, TypeSign y, TypeSign z) =>
    TypesSubtraction (x :: a) (y :: b) (z :: n) where
        type (-) (x :: a) (y :: b) :: n

-----------------------------------------------------------------------------

class (TypesNat x x x, TypesNat y y y, TypesNat z z z, TypesNat x y z) =>
    TypesRational (x :: a) (y :: b) (z :: n) where
        type (/) (x :: a) (y :: b) :: n


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


instance TypesEq (a :: Sign)     (b :: Sign)     where type a ~~ b = SameSign a b
type instance a == b = SameSign a b

-----------------------------------------------------------------------------

