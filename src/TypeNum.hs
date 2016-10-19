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
--, HasZero(..), IsZero(..), IsZeroFunc, NotZero(..), NotZeroFunc
--, HasOne(..), IsOne(..), IsOneFunc, NotOne(..), NotOneFunc

, TypeNat(..)
, TypeIntegral(..)
, TypeAbsDiff(..)
, TypeSign(..)
, TypeSubtraction(..)
, TypeRational(..)

--, Convertible(..)

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

--class HasZero (x :: a) where type GetZero x :: a
--
--type family IsZero x where IsZero x = x == GetZero x
--data IsZeroFunc x (b :: Bool)
--type instance IsZeroFunc :$: x = IsZero x
--
--type family NotZero x where NotZero x = Not (IsZero x)
--data NotZeroFunc x (b :: Bool)
--type instance NotZeroFunc :$: x = NotZero x

-----------------------------------------------------------------------------

--class HasOne (x :: a) where type GetOne x :: a
--
--type family IsOne x where IsOne x = x == GetOne x
--data IsOneFunc x (b :: Bool)
--type instance IsOneFunc :$: x = IsOne x
--
--type family NotOne x where NotOne x = Not (IsOne x)
--data NotOneFunc x (b :: Bool)
--type instance NotOneFunc :$: x = NotOne x


-----------------------------------------------------------------------------

-- | Natural-like numbers operations.
class (TypesOrd x y, TypesOrd x x, TypesOrd y y) =>
    TypeNat (x :: n) (y :: n) where
        type (+) x y :: n
        type (*)  x y :: n
        type (^)  x y :: n


-----------------------------------------------------------------------------

class (TypeNat x y) => TypeAbsDiff (x :: a) (y :: a) where
        -- | Absolute difference.
        type (/-) x y :: n

-----------------------------------------------------------------------------

-- | Integral numbers, supporting integer division.
class (TypeNat x y) =>
    TypeIntegral (x :: n) (y :: n) where
        -- | Integer division truncated toward zero.
        type Quot x y :: n
        -- | Integer remainder.
        type Rem x y :: n
        -- | Simultaneous 'Quot' and 'Rem'.
        type QuotRem x y :: (n, n)

        -- | Integer division truncated toward negative infinity.
        type Div x y :: n
        -- | Integer modulus.
        type Mod x y :: n
        -- | Simultaneous 'Div' and 'Mod'.
        type DivMod x y :: (n, n)

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

class (TypeSign x, TypeSign y) =>
    TypeSubtraction (x :: n) (y :: n) where type (-) x y :: n

-----------------------------------------------------------------------------

class (TypeNat x y) =>
    TypeRational (x :: n) (y :: n)    where type (/) x y :: n


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


instance TypesEq (a :: Sign) (b :: Sign) where type a ~~ b = SameSign a b
type instance a == b = SameSign a b

-----------------------------------------------------------------------------

class Convertible (x :: a) (y :: b) where type Convert x :: b

-----------------------------------------------------------------------------

