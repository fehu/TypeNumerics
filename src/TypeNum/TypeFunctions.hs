-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.TypeFunctions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


{-# LANGUAGE PolyKinds, ConstraintKinds, FunctionalDependencies #-}

module TypeNum.TypeFunctions (

  TypesEq(..), TypesOrd(..)

-- Apply type-level function
, (:$:)

-- * Curry operations

, Curry , Uncurry

-- * Tuple deconstruction
, Fst, Snd

-- * Arrow-like tuple operations
, First, Second

-- * Different tuple operations
, ZipPairs, Firsts, Seconds


-- Some type functions
, CmpFunc

) where

import Data.Type.Bool
import Data.Type.Equality

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypesEq  (x :: a) (y :: b) where
    -- | Types equality, permitting types of different kinds.
    type (~~) (x :: a) (y :: b) :: Bool


type (=~=) a b = (a ~~ b) ~ True
type (/~=) a b = (a ~~ b) ~ False

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

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

        type a <= b = a ~~ b || a < b
        type a >= b = a ~~ b || a > b



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------



instance TypesEq (a :: Ordering) (b :: Ordering) where type a ~~ b = a == b

-----------------------------------------------------------------------------

instance TypesEq (a :: (x,y)) (b :: (x,y)) where
    type a ~~ b = (Fst a == Fst b) && (Snd a == Snd b)

instance TypesOrd (a :: (x,y)) (b :: (x,y)) where
    type Cmp a b = Cmp2 a b




-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family Fst (p :: (a, b)) :: a where Fst '(a, b) = a
type family Snd (p :: (a, b)) :: b where Snd '(a, b) = b

-----------------------------------------------------------------------------

type family Cmp2 (a :: (x,y)) (b :: (x,y)) :: Ordering where
    Cmp2 '(x1, y1) '(x2, y2) = If (x1 == x2) (Cmp y1 y2) (Cmp x1 x2)



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


-- Apply a type-level function. Extendable.
type family (:$:) (f :: a -> b -> *) (x :: a) :: b



-----------------------------------------------------------------------------

type family Curry (f :: (a,b) -> c -> *) (x :: a) (y :: b) :: c
    where Curry f x y = f :$: '(x, y)

type family Uncurry (f :: a -> (b -> c -> *) -> *) (p :: (a,b)) :: c
    where Uncurry f '(x,y) = (f :$: x) :$: y


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Arrow-like tuple operations


type family First (p :: (a,b)) f :: (c,b) where
    First '(a,b) f = '(f :$: a, b)

type family Second (p :: (a,b)) f :: (a,c) where
    Second '(a,b) f = '(a, f :$: b)



-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family ZipPairs (p1 :: (a,a)) (p2 :: (b,b)) :: ((a,b), (a,b)) where
    ZipPairs '(a1, a2) '(b1, b2) = '( '(a1,b1), '(a2,b2))


type family Firsts (p :: ((a,b), (a,b))) f :: ((c,b), (c,b))
    where Firsts '( '(a1,b1), '(a2,b2)) f = ZipPairs (f :$: '(a1, a2)) '(b1,b2)

type family Seconds (p :: ((a,b), (a,b))) f :: ((a,c), (a,c))
    where Seconds '( '(a1,b1), '(a2,b2)) f = ZipPairs '(a1,a2) (f :$: '(b1, b2))


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


data CmpFunc (c :: (a,b)) (o :: Ordering)
type instance CmpFunc :$: '(x,y) = Cmp x y


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- Tests

data A = A'
data B = B'
data C = C'

data ACFunc a c
type instance ACFunc :$: A' = C'

type X = First '(A', B') ACFunc

data ACFunc2 a c
type instance ACFunc2 :$: '(A', A') = '(C', C')

type Y = Firsts '( '(A',B'), '(A',B')) ACFunc2

