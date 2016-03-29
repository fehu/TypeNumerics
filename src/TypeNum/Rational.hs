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

{-# LANGUAGE Rank2Types
           , MultiParamTypeClasses
           , FlexibleInstances
           , PolyKinds
           , ConstraintKinds
           , FlexibleContexts
       #-}

module TypeNum.Rational(

  TRational(..), Rational'
, (:%), (:%:)
, (%:)
, rationalValue, rational

, IntValuesForRational
, AsRational(..)

) where


import TypeNum.Integer
import TypeNum.Integer.Positive

import GHC.TypeLits
import GHC.Real

import Data.Type.Equality

-----------------------------------------------------------------------------


data TRational (numerator :: TInt) (denominator :: PosInt) = TRational (Int' numerator)
                                                                       (PosInt' denominator)


type family (:%:) (num :: TInt) (den :: Nat) where n :%: d = TRational n (PositiveUnsafe d)

type family (:%) (num :: TInt) (den :: Nat) where
    n :% 0 = Nothing
    n :% d = Just (n :%: d)

type Rational' (num :: TInt) (den :: Nat) = ((den == 0) ~ False) =>
     TRational num (PositiveUnsafe den)


(%:) :: Int' num -> PosInt' den -> TRational num den
n %: d = TRational n d

rationalValue :: (TIntValue n, TIntValue d, Positive2Int d' ~ d) => TRational n d' -> Rational
rationalValue (TRational n d) = intValue n :% posIntValue d

rational :: TRational n d
rational = TRational Int' PosInt'


instance (TIntValue n, TIntValue d, Positive2Int d' ~ d) => Show (TRational n d')
    where show (TRational n d) = let d' = posIntValue d
                                 in if d' == 1 then show n
                                               else "(" ++ show n ++ "/" ++ show d' ++ ")"

-----------------------------------------------------------------------------

type family Fst (p :: (TInt, PosInt)) :: TInt   where Fst '(x,y) = x
type family Snd (p :: (TInt, PosInt)) :: PosInt where Snd '(x,y) = y

type Uncurry (a :: TInt -> PosInt -> *) (p :: (TInt, PosInt)) = a (Fst p) (Snd p)


type IntValuesForRational a = (TIntValue (Fst a), TIntValue (Positive2Int (Snd a)))


class AsRational a where type AsRational' a :: (TInt, PosInt)
                         asRational :: (IntValuesForRational (AsRational' a)) =>
                                        a -> Uncurry TRational (AsRational' a)
                         asRational' :: a


instance AsRational (Int' i) where type AsRational' (Int' i) = '(i, One)
                                   asRational i = i %: positive (Nat' :: Nat' 1)
                                   asRational' = Int'

instance AsRational (TRational n d) where type AsRational' (TRational n d) = '(n, d)
                                          asRational = id
                                          asRational' = rational


-----------------------------------------------------------------------------
