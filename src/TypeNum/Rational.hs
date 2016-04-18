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

  Rational'(..), Ratio'
, (:%), (:%:)
, (%:)
, rationalValue, rational

, IntValuesForRational
, AsRational(..)

--, TRational
--, RatioOf

, module TypeNum.Integer
, module TypeNum.Integer.Positive
, TypesRational(..)

) where

import TypeNum
import TypeNum.Integer
import TypeNum.Integer.Positive

import GHC.TypeLits
import GHC.Real

import Data.Type.Equality as Eq

-----------------------------------------------------------------------------

data Rational' (num :: TInt) (den :: PosInt) = Rational' (Int' num) (PosInt' den)

-- Another constructor for Rational'

type Ratio' (num :: TInt) (den :: Nat) = ((den Eq.== 0) ~ False) => Rational' num (PositiveUnsafe den)

-----------------------------------------------------------------------------

type family (:%:) (num :: TInt) (den :: Nat) where n :%: d = Rational' n (PositiveUnsafe d)

type family (:%) (num :: TInt) (den :: Nat) where
    n :% 0 = Nothing
    n :% d = Just (n :%: d)



(%:) :: Int' num -> PosInt' den -> Rational' num den
n %: d = Rational' n d

rationalValue :: (TIntValue n, TIntValue d, Positive2Int d' ~ d) => Rational' n d' -> Rational
rationalValue (Rational' n d) = intValue n :% posIntValue d

rational :: Rational' n d
rational = Rational' Int' PosInt'

-----------------------------------------------------------------------------

instance (TIntValue n, TIntValue d, Positive2Int d' ~ d) => Show (Rational' n d')
    where show (Rational' n d) = let d' = posIntValue d
                                 in if d' == 1 then show n
                                               else "(" ++ show n ++ "/" ++ show d' ++ ")"

-----------------------------------------------------------------------------

type Uncurry (a :: TInt -> PosInt -> *) (p :: (TInt, PosInt)) = a (Fst p) (Snd p)


type IntValuesForRational a = (TIntValue (Fst a), TIntValue (Positive2Int (Snd a)))


-----------------------------------------------------------------------------

class AsRational a where type AsRational' a :: (TInt, PosInt)
                         asRational :: (IntValuesForRational (AsRational' a)) =>
                                        a -> Uncurry Rational' (AsRational' a)
                         asRational' :: a


instance AsRational (Int' i) where type AsRational' (Int' i) = '(i, One)
                                   asRational i = i %: positive (Nat' :: Nat' 1)
                                   asRational' = Int'

instance AsRational (Rational' n d) where type AsRational' (Rational' n d) = '(n, d)
                                          asRational = id
                                          asRational' = rational


-----------------------------------------------------------------------------

--instance




-----------------------------------------------------------------------------
