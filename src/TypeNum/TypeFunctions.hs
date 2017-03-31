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


{-# LANGUAGE PolyKinds, ConstraintKinds #-}

module TypeNum.TypeFunctions (

-- * Types equality
  TypesEq(..) , type (==)
, type (=~=), type (/~=)

-- * Types ordering
, TypesOrd(..) -- , Cmp
, type (<), type (>), type (<=), type (>=)
, Min, Max

-- * Pairs deconstruction
, Fst, Snd, ZipPairs

-- * Maybe
, FromMaybe

-- * Arrow-like tuple operations
, First, Second

-- * Different tuple operations
, Firsts, Seconds

-- * Containers
, TContainerElem(..) -- Contains, All, Any, Prepend, Append, Rm
, type (++), ContainsEach

, TContainers(..) -- , Concat, SameSize

, TContainerSameSize(..) -- , Zip

-- -- * Folds
-- , Fold, FoldWhile

-- * Functions
, (:$:)
, Curry, Uncurry

-- * Some :$: functions
, EqFunc, EqualFunc, CmpFunc
, ContainsFunc

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


type family (x :: a) < (y :: b)  where a < b = Cmp a b == LT
type family (x :: a) > (y :: b)  where a > b = Cmp a b == GT
type family (x :: a) <= (y :: b) where a <= b = a ~=~ b || a < b
type family (x :: a) >= (y :: b) where a >= b = a ~=~ b || a > b


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
type instance EqualFunc x :$: y  =  x ~=~ y



data CmpFunc (c :: (a,b)) (o :: Ordering)
type instance CmpFunc :$: '(x,y) = Cmp x y

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class TContainerElem (t :: k) (x :: e) where
  type Contains x t :: Bool
  type All (cond :: e -> Bool -> *) t :: Bool
  type Any (cond :: e -> Bool -> *) t :: Bool

  type Prepend x t :: k
  type Append  t x :: k
  type Rm      x t :: k

class TContainers (t1 :: k) (t2 :: k) where
  type Concat   t1 t2 :: k
  type SameSize t1 t2 :: Bool

-- class (SameSize t1 t2 ~ True) =>
--   TContainerSameSize (t1 :: k) (t2 :: k) where
--     type Zip t1 t2 :: k

class (SameSize t1 t2 ~ True) =>
  TContainerSameSize (t1 :: k) (t2 :: k) (res :: k') where
    type Zip t1 t2 :: k'


data ContainsFunc  (t :: k) (x :: e) (res :: Bool)
type instance ContainsFunc t :$: x = Contains x t

type ContainsEach (t :: k) (xs :: k) = All (ContainsFunc t) xs

type xs ++ ys = Concat xs ys

-----------------------------------------------------------------------------

class FoldableT (t :: k) (x :: e) (x0 :: r) where
  type Fold (f :: (r, e) -> r -> *) x0 t :: r
  type FoldWhile (cond :: e -> Bool -> *) (f :: (r, e) -> r -> *) x0 t :: r

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance TContainerElem ('[] :: [a]) (x :: a) where
  type Contains x '[] = False
  type All cond   '[] = True
  type Any cond   '[] = False
  type Prepend x '[]  = '[x]
  type Append  '[] x  = '[x]
  type Rm      x '[]  = '[]

instance TContainerElem ((h ': t) :: [a]) (x :: a) where
  type Contains x (h ': t) = h == x || Contains x t
  type All cond   (h ': t) = (cond :$: h) && All cond t
  type Any cond   (h ': t) = (cond :$: h) || Any cond t
  type Prepend x (h ': t)   = x ': h ': t
  type Append    (h ': t) x = h ': Append t x
  type Rm      x (h ': t)   = If (h == x) (Rm x t) (h ': Rm x t)

-----------------------------------------------------------------------------

instance TContainers ('[] :: [a]) ('[] :: [a]) where
  type Concat   '[] '[] = '[]
  type SameSize '[] '[] = True

instance TContainers ('[] :: [a]) ((h ': t) :: [a]) where
  type Concat   '[] (h ': t) = (h ': t)
  type SameSize '[] (h ': t) = False

instance TContainers ((h ': t) :: [a]) ('[] :: [a]) where
  type Concat   (h ': t) '[] = (h ': t)
  type SameSize (h ': t) '[] = False

instance TContainers ((h1 ': t1) :: [a]) ((h2 ': t2) :: [a]) where
  type Concat   (h1 ': t1) (h2 ': t2) = h1 ': Concat t1 (h2 ': t2)
  type SameSize (h1 ': t1) (h2 ': t2) = SameSize t1 t2

-----------------------------------------------------------------------------

instance TContainerSameSize ('[] :: [a]) ('[] :: [a]) ('[] :: [(a,a)]) where
  type Zip '[] '[] = '[]

instance (SameSize t1 t2 ~ True) => TContainerSameSize ((h1 ': t1) :: [a])
                                                       ((h2 ': t2) :: [a])
                                                       (res :: [(a,a)])
  where
    type Zip (h1 ': t1) (h2 ': t2) =  '(h1, h2) ': Zip t1 t2

-----------------------------------------------------------------------------

instance FoldableT ('[] :: [a]) (x :: a) x0 where
  type Fold           f x0 '[] = x0
  type FoldWhile cond f x0 '[] = x0


instance FoldableT ((h ': t) :: [a]) (x :: a) x0 where
  type Fold           f x0 (x ': xs) = Fold f (f :$: '(x0, x)) xs
  type FoldWhile cond f x0 (x ': xs) = If (cond :$: x)
                                          (FoldWhile cond f (f :$: '(x0, x)) xs)
                                          x0

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
