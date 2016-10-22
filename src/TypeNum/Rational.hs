-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Rational
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts
           , PolyKinds
       #-}

module TypeNum.Rational(

  TRational, (:%), Rational'
, KnownRatio(..), Ratio'(..)

, MaybeRational(..)

, module TypeNum.Integer
, module TypeNum.Integer.Positive

) where

import TypeNum
import TypeNum.Integer
import TypeNum.Integer.Positive

import GHC.Real

import Data.Type.Bool


-- | Kind-promotable rational inner representation.
--   The integer part of the number always must be extracted,
--   so that numerator < denominator.

data TRational = TRational' TInt TInt TInt    -- ^ Whole part, numerator, denominator

-- | 'TRational' type container.
data (KnownRatio r) => Ratio' (r :: TRational) = Ratio'


class   ( TIntValue (IntegerPart r)
        , TIntValue (Numerator r)
        , PosIntValue (Denominator r)) =>
      KnownRatio r where type IntegerPart r  :: TInt
                         type Numerator r    :: TInt
                         type Denominator r  :: PosInt

                         integerPart  :: Ratio' r -> Int' (IntegerPart r)
                         numerator'   :: Ratio' r -> Int' (Numerator r)
                         denominator' :: Ratio' r -> PosInt' (Denominator r)

                         integerPart  = const Int'
                         numerator'   = const Int'
                         denominator' = const PosInt'


instance ( TIntValue i, TIntValue n, TIntValue d
         , PosIntValue (Int2Positive d)) =>
    KnownRatio (TRational' i n d) where
        type IntegerPart (TRational' i n d) = i
        type Numerator   (TRational' i n d) = n
        type Denominator (TRational' i n d) = Int2Positive d


-----------------------------------------------------------------------------
-- Tried to simplify ratio, but the compiler enters infinite recursion.
-----------------------------------------------------------------------------

--type family NextPrime (pool :: [TInt]) (prev :: [TInt]) :: Maybe (TInt, [TInt])
--  where
--    NextPrime (h ': t) prev = If (Any (ZeroRemFunc h) prev)
--                                 (NextPrime t prev)
--                                 (Just '(h, t))
--
--type family FilterPrimes (pool :: [TInt]) (prev :: [TInt]) :: [TInt] where
--    FilterPrimes pool prev = FromMaybe (NextPrime pool prev)
--                                       (FilterPrimesFunc prev)
--                                       prev
--
--type family IntRange (from :: TInt) (to :: TInt) :: [TInt] where
--    IntRange from to = If (from < to)
--                          (from ': IntRange (Succ from) to)
--                          '[to]
--
--data FilterPrimesFunc (prev :: [TInt]) (arg :: (TInt, [TInt])) (res :: [TInt])
--type instance (FilterPrimesFunc prev) :$: '(prime,pool) = FilterPrimes pool (Concat prev '[prime])
--
--data ZeroRemFunc (den :: TInt) (num :: TInt) (res :: Bool)
--type instance (ZeroRemFunc den) :$: num = Rem num den ~~ 0
--
--type Two = Succ (Succ Zero)
--
--type family PrimeNumbersTill (max :: a) :: [a] where
--    PrimeNumbersTill max = FilterPrimes (IntRange Two max) '[Two]


-----------------------------------------------------------------------------

-- The simplification of ratio 'n//d' is done as follows (but at type-level):
--   d' <- d
--   n' <- n
--   forall i in 'prime numbers', while i <= d' do
--      if d `rem` i == 0 && n `rem` i == 0
--      then  d' <- d' `quot` i; n' <- n' `quot` i; use same i
--      else leave d' and n' unchanged

--type family SimplifyRatio (n :: TInt) (d :: TInt) :: (TInt,TInt) where
--    SimplifyRatio n d =
--        -- If numerator is zero, then there is no ratio part to simplify.
--        If (n ~~ 0) '(Zero, Pos 1) (SimplifyRatio' n d)
--
--type family SimplifyRatio' (n :: a) (d :: a) :: (a,a) where
--    SimplifyRatio' n d = Fold SimplifyFunc '(n,d) (PrimeNumbersTill (Max n d))
--
--data SimplifyFunc (arg :: (a,b)) (res :: a)
--type instance SimplifyFunc :$: '( '(n,d), i ) = RepeatQuot i n d
--
--type family RepeatQuot i n d where
--    RepeatQuot i n d  = If (Rem n i == 0 && Rem d i == 0)
--                           (RepeatQuot i (Quot n i) (Quot d i))
--                           '(n,d)
--
--type TRational'' i r = TRational' i (Fst r) (Snd r)
--
--type SimplifiedRatio n d = TRational'' (Quot n d) -- integer part
--                                       (SimplifyRatio n d)
--
---- | Programmer-friendly rational type constructor.
--type (:%) (n :: TInt) (d :: Nat) = SimplifiedRatio n (Pos d)
--
---- | Accessible rational type constructor.
--type Rational' (int :: TInt) (num :: TInt) (den :: PosInt) =
--    AsRational int + SimplifiedRatio num (Positive2Int den)



-- | Programmer-friendly rational type constructor.
type (:%) (n :: TInt) (d :: Nat) = TRational' (Quot n (Pos d)) (Rem n (Pos d)) (Pos d)

type MkRational (i :: TInt) (n :: TInt) (d :: TInt) =
    TRational' (i + Quot n d) (Rem n d) d


-- | Accessible rational type constructor.
type Rational' (int :: TInt) (num :: TInt) (den :: PosInt) =
    AsRational int + (num :% (Positive2Nat den))

-----------------------------------------------------------------------------

class MaybeRational (a :: k) where type AsRational a :: TRational
                                   asRational :: NumContainer a a -> Ratio' (AsRational a)
                                   asRational = const Ratio'

instance MaybeRational (TRational' i n d) where type AsRational (TRational' i n d) = TRational' i n d

instance MaybeRational (i :: TInt)   where type AsRational i = TRational' i                 Zero (Succ Zero)
instance MaybeRational (i :: PosInt) where type AsRational i = TRational' (Positive2Int i)  Zero (Succ Zero)

--type family AsRational (a :: k) :: TRational
--
--type instance AsRational (TRational' i n d) = TRational' i n d
--
--type instance AsRational (i :: TInt)    = TRational' i Zero One
--type instance AsRational (i :: PosInt)  = TRational' (Positive2Int i) Zero One

-----------------------------------------------------------------------------

-- No LCD optimisation is done on underlying ratios comparison.
type family CmpRational (r1 :: TRational) (r2 :: TRational) :: Ordering where
    CmpRational (TRational' i1 n1 d1) (TRational' i2 n2 d2) =
        Cmp  [i1, n1* d2]
             [i2, n2* d1]

-----------------------------------------------------------------------------

instance (KnownRatio (TRational' i n d)) =>
  TypeNumValue (TRational' i n d) where
    type NumValue     (TRational' i n d) = Rational
    type NumContainer (TRational' i n d) = Ratio'
    runtimeValue r = (int*den+num) :% den
        where int = intValue $ integerPart r
              num = intValue $ numerator' r
              den = posIntValue $ denominator' r

type family RationalEq (r1 :: TRational) (r2 :: TRational) where
    RationalEq (TRational' i1 n1 d1) (TRational' i2 n2 d2) =
        i1 == i2 && n1*d2 == n2*d1

-- The simpified rational numbers equal when integer parts, numerators and denominators are equal.
instance TypesEq (r1 :: TRational) (r2 :: TRational) where type r1 ~~ r2 = r1 == r2
type instance a == b = RationalEq a b -- TRationalAsList a == TRationalAsList b


instance TypesEq (r :: TRational) (i :: TInt) where type r ~~ i  = Numerator r == Zero
                                                                && IntegerPart r == i

instance TypesEq (i :: TInt) (r :: TRational) where type i ~~ r = r ~~ i

-- The comparison is done lexicographically: first whole parts, then remaning ratios.
instance TypesOrd (r1 :: TRational) (r2 :: TRational) where type Cmp r1 r2 = CmpRational r1 r2

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

type family TRationalSum (a :: TRational) (b :: TRational) :: TRational where
    TRationalSum (TRational' i1 n1 d1) (TRational' i2 n2 d2) =
        MkRational (i1+i2) (n1*d2 + n2*d1) (d1*d2)

--type family TRationalSum (a :: TRational) (b :: TRational) :: TRational where
--    TRationalSum (TRational' i1 Zero d1) (TRational' i2 Zero d2)  = TRational' (i1 + i2) Zero (Pos 1)
--    TRationalSum (TRational' i1 Zero d1) (TRational' i2 n2   d2)  = TRational' (i1 + i2) n2 d2
--    TRationalSum (TRational' i1 n1   d1) (TRational' i2 Zero d2)  = TRational' (i1 + i2) n1 d1
--    TRationalSum (TRational' i1 n1   d1) (TRational' i2 n2   d2)  = AsRational (i1+i2)
--                                                                  + SimplifiedRatio (n1*d2 + n2*d1)
--                                                                                    (d1*d2)


instance TypeNat (a :: TRational) (b :: TRational) where
    type x + y = TRationalSum x y


