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
           , ConstraintKinds
           , FlexibleContexts
       #-}

module TypeNum.Rational(

  TRational(..)
, Ratio'(..)

, RationalK, Rational', Rational''

, numerator', denominator', KnownRatio

, AsRational(..)

, module TypeNum.Integer
, module TypeNum.Integer.Positive
, TypesRational(..)

) where

import TypeNum
import TypeNum.Integer
import TypeNum.Integer.Positive

import GHC.Real

import Data.Type.Bool

-----------------------------------------------------------------------------

-- | Promotable rational representation.
data TRational = TRational' TInt PosInt

-- | 'TRational' type container.
data Ratio' (r :: TRational) = Ratio'

-- | 'TRational' kind constructor.
type family RationalK (num :: TInt) (den :: PosInt) where
    RationalK n d = TRational' n d


-- | TRational type constructor.
type Rational' (num :: TInt) (den :: PosInt) = Ratio' (RationalK num den)

-- | Alternative TRational type constructor.
type Rational'' (num :: TInt) (den :: Nat) = (den /== 0) => Rational' num (PositiveUnsafe den)


-----------------------------------------------------------------------------

instance TypesEq (TRational' n1 d1) (TRational' n2 d2) where
    type (TRational' n1 d1) == (TRational' n2 d2) = n1 == n2 && d1 == d2

instance TypesEq (TRational' n d) (i :: TInt) where
    type (TRational' n d) == i = (QuotRem n d) == '(i, 0)


instance TypesOrd (TRational' n1 d1) (TRational' n2 d2) where
    type Cmp (TRational' n1 d1) (TRational' n2 d2) = Cmp (QuotRem n1 d1)
                                                         (QuotRem n2 d2)

instance TypesOrd (TRational' n d) (i :: TInt) where
    type Cmp (TRational' n d) (i :: TInt) = Cmp (QuotRem n d) '(i,0)


numerator' :: Ratio' (TRational' n d) -> Int' n
numerator' _ = Int'

denominator' :: Ratio' (TRational' n d) -> PosInt' d
denominator' _ = PosInt'

type KnownRatio n d = (TIntValue n, TIntValue (Positive2Int d))

instance (KnownRatio n d) =>
    TypeNumValue (TRational' n d) where type NumValue (TRational' n d) = Rational
                                        type NumContainer (TRational' n d) = Ratio'
                                        runtimeValue r = intValue (numerator' r)
                                                      :% posIntValue (denominator' r)

instance (TIntValue n) =>
    Show (Ratio' (TRational' n One)) where show = show . numerator'

instance (KnownRatio n d) =>
    Show (Ratio' (TRational' n d)) where
        show r = "{" ++ show (numerator' r) ++ "/" ++ show (denominator' r) ++ "}"

-----------------------------------------------------------------------------

class AsRational a where type AsRational' a :: TRational
                         asRational  :: a -> Ratio' (AsRational' a)
                         asRational' :: a

                         asRational _ = Ratio'

instance AsRational (Int' i) where type AsRational' (Int' i) = TRational' i One
                                   asRational'  = Int'

instance AsRational (Ratio' r) where type AsRational' (Ratio' r) = r
                                     asRational'  = Ratio'


-----------------------------------------------------------------------------



