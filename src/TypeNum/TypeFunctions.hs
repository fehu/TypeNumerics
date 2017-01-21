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

  TypesEq(..), type (=~=), type  (/~=)
, TypesOrd(..)

, Max, Min

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

-- * Folds
, Fold, FoldWhile

-- * List functions
, Concat, Zip

, All, Any
, Contains

-- * Maybe functions
, FromMaybe

-- Some type functions
, EqFunc, EqualFunc
, CmpFunc

) where

import Data.Type.Bool
import Data.Type.Equality

-----------------------------------------------------------------------------

infix 4 ~=~, =~=, /~=
infix 4 <, <=, >, >=

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TypesEq  (x :: a) (y :: b) where
    -- | Types equality, permitting types of different kinds.
    type (~=~) (x :: a) (y :: b) :: Bool


type (=~=) a b = (a ~=~ b) ~ True
type (/~=) a b = (a ~=~ b) ~ False

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

        type a <= b = a ~=~ b || a < b
        type a >= b = a ~=~ b || a > b


type Max (x :: a) (y :: b) = If (x >= y) x y
type Min (x :: a) (y :: b) = If (x <= y) x y

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------



instance TypesEq (a :: Ordering) (b :: Ordering) where type a ~=~ b = a == b

-----------------------------------------------------------------------------

instance TypesEq (a :: (x,y)) (b :: (x,y)) where
    type a ~=~ b = (Fst a == Fst b) && (Snd a == Snd b)

instance TypesOrd (a :: (x,y)) (b :: (x,y)) where
    type Cmp a b = Cmp2 a b

-----------------------------------------------------------------------------

instance TypesEq (a :: [x]) (b :: [x]) where
    type a ~=~ b = All EqFunc (Zip a b)

-- Lexicographical order.
instance TypesOrd (h1 ': t1 :: [x]) (h2 ': t2 :: [x]) where
    type Cmp (h1 ': t1) (h2 ': t2) = CmpTypesOrd (Cmp h1 h2) t1 t2

type family CmpTypesOrd (o :: Ordering) (a :: [x]) (b :: [x]) :: Ordering where
    CmpTypesOrd EQ (h1 ': t1) (h2 ': t2) = CmpTypesOrd (Cmp h1 h2) t1 t2
    CmpTypesOrd EQ '[] '[] = EQ
    CmpTypesOrd EQ '[]  l2 = LT
    CmpTypesOrd EQ  l1 '[] = GT
    CmpTypesOrd o   l1  l2 = o

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

data EqFunc (arg :: (a,b)) (res :: Bool)
type instance EqFunc :$: '(x,y) = x ~=~ y

data EqualFunc (val :: a) (arg :: b) (res :: Bool)
type instance (EqualFunc x) :$: y = x ~=~ y



data CmpFunc (c :: (a,b)) (o :: Ordering)
type instance CmpFunc :$: '(x,y) = Cmp x y

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family Fold f r0 l where
    Fold f res '[]      = res
    Fold f res (h ': t) = Fold f (f :$: '(res, h)) t

type family FoldWhile cond f r0 l where
    FoldWhile cond f res '[]      = res
    FoldWhile cond f res (h ': t) = If (cond :$: h)
                                       (FoldWhile cond f (f :$: (res, h)) t)
                                       res

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family Concat (l1 :: [a]) (l2 :: [a]) :: [a] where
    Concat '[] l2      = l2
    Concat (h ': t) l2 = h ': Concat t l2


type family All cond (l :: [a]) :: Bool where
    All cond '[]      = True
    All cond (h ': t) = cond :$: h && All cond t

type family Any cond (l :: [a]) :: Bool where
    Any cond '[]      = False
    Any cond (h ': t) = cond :$: h || Any cond t


type family Zip (l1 :: [a]) (l2 :: [b]) :: [(a,b)] where
    Zip (h1 ': t1) (h2 ': t2) = '(h1,h2) ': Zip t1 t2


type Contains (l :: [a]) (x :: a) = Any (EqualFunc x) l

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family FromMaybe (mb :: Maybe b) f (x0 :: a) :: a where
    FromMaybe (Just x) f x0 = f :$: x
    FromMaybe Nothing  f x0 = x0

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
