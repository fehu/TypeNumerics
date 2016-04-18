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

module TypeNum  where

import Data.Type.Bool
import qualified Data.Type.Equality as Eq

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

class TypesEq  (x :: a) (y :: b) where
    -- | Types equality.
    type (==) (x :: a) (y :: b) :: Bool

class (TypesEq x y) =>
    TypesOrd (x :: a) (y :: b) where
        -- | Types ordering.
        type Cmp  (x :: a) (y :: b) :: Ordering

        type (<)  (x :: a) (y :: b) :: Bool
        type (>)  (x :: a) (y :: b) :: Bool
        type (<=) (x :: a) (y :: b) :: Bool
        type (>=) (x :: a) (y :: b) :: Bool

        type a < b = Cmp a b == LT
        type a > b = Cmp a b == GT

        type a <= b = a == b || a < b
        type a >= b = a == b || a > b

type (===) a b = (a == b) ~ True
type (/==) a b = (a == b) ~ False

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

class (TypeSign x, TypeSign y, TypeSign z) =>
    TypesSubtraction (x :: a) (y :: b) (z :: n) where
        type (-) (x :: a) (y :: b) :: n

class (TypesNat x x x, TypesNat y y y, TypesNat z z z, TypesNat x y z) =>
    TypesRational (x :: a) (y :: b) (z :: n) where
        type (/) (x :: a) (y :: b) :: n


-----------------------------------------------------------------------------

type family Fst (p :: (a, b)) :: a where Fst '(a, b) = a
type family Snd (p :: (a, b)) :: b where Snd '(a, b) = b


instance TypesEq (a :: Ordering) (b :: Ordering) where type a == b = a Eq.== b
instance TypesEq (a :: Sign)     (b :: Sign)     where type a == b = SameSign a b

instance TypesEq (a :: (x,y)) (b :: (x,y)) where type a == b = (Fst a == Fst b) && (Snd a == Snd b)



