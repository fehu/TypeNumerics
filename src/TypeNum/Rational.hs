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
           , PolyKinds
       #-}

module TypeNum.Rational(

  TRational(..)
, Ratio'(..), (:%)

, KnownRatio(..)
, MayRational(..)

, module TypeNum.Integer
, module TypeNum.Integer.Positive
, TypesRational(..)

) where

import TypeNum
import TypeNum.Integer
import TypeNum.Integer.Positive

import GHC.Real

import Data.Type.Bool
import Data.Type.Equality

-----------------------------------------------------------------------------

-- | Promotable rational representation.
data TRational = TRational' TInt PosInt
               | TRationalIncorrect

-- | 'TRational' type container.
data (KnownRatio r) => Ratio' (r :: TRational) = Ratio'

-- | Rational short constructor.
type family (:%) (num :: TInt) (den :: Nat) :: TRational
    where (:%) n 0 = TRationalIncorrect
          (:%) n d = TRational' n (Nat2Positive d)



-----------------------------------------------------------------------------

instance TypesEq (TRational' n1 d1) (TRational' n2 d2) where
    type (TRational' n1 d1) ~~ (TRational' n2 d2) =
            Cmp (TRational' n1 d1) (TRational' n2 d2) == EQ

instance TypesEq (TRational' n d) (i :: TInt) where
    type (TRational' n d) ~~ i = QuotRem n (Positive2Int d) == '(i, Zero)

type instance (a :: TRational) == b = a ~~ b

instance TypesOrd (TRational' n1 d1) (TRational' n2 d2) where
    type Cmp (TRational' n1 d1) (TRational' n2 d2) =
        CmpFunc :$: Seconds '(QuotRem n1 (Positive2Int d1), QuotRem n2 (Positive2Int d2) )
                             (XMultiply d1 d2)


data XMultiply (d1 :: PosInt) (d2 :: PosInt) (m :: (TInt,TInt)) (r :: (TInt,TInt))
type instance (XMultiply d1 d2) :$: '(n1,n2) = '(n1*(Positive2Int d2), n2*(Positive2Int d1))


instance TypesOrd (TRational' n d) (i :: TInt) where
    type Cmp (TRational' n d) (i :: TInt) = Cmp (QuotRem n d) '(i,0)


-----------------------------------------------------------------------------

class (TIntValue (Numerator r), PosIntValue (Denominator r)) =>
      KnownRatio (r :: TRational) where type Numerator r :: TInt
                                        type Denominator r :: PosInt

                                        numerator'   :: Ratio' r -> Int' (Numerator r)
                                        denominator' :: Ratio' r -> PosInt' (Denominator r)

                                        numerator'   = const Int'
                                        denominator' = const PosInt'


instance (TIntValue n, PosIntValue d) =>
     KnownRatio (TRational' n d) where type Numerator (TRational' n d) = n
                                       type Denominator (TRational' n d) = d

-----------------------------------------------------------------------------

instance (KnownRatio (TRational' n d)) =>
    TypeNumValue (TRational' n d) where type NumValue (TRational' n d) = Rational
                                        type NumContainer (TRational' n d) = Ratio'
                                        runtimeValue r = intValue (numerator' r)
                                                      :% posIntValue (denominator' r)

instance (KnownRatio r) =>
    Show (Ratio' r) where
        show r = let num = show (numerator' r)
                     den = show (denominator' r)
                 in case posIntValue $ denominator' r of 1 -> num
                                                         d -> "{" ++ num ++ "/" ++ den ++ "}"

-----------------------------------------------------------------------------

class MayRational (a :: k) where type AsRational a :: TRational
                                 type MRationalC a :: k -> *
                                 asRational :: (MRationalC a) a -> Ratio' (AsRational a)

instance MayRational (r :: TRational) where type AsRational r = r
                                            type MRationalC r = Ratio'
                                            asRational = id

instance MayRational (n :: Nat) where type AsRational n = Pos n :% 1
                                      type MRationalC n = Nat'
                                      asRational n = Ratio'

instance MayRational (n :: TInt) where type AsRational n = n :% 1
                                       type MRationalC n = Int'
                                       asRational n = Ratio'


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance TypesNat (TRational' n1 d1) (TRational' n2 d2) (TRational' n3 d3) where
    type (TRational' n1 d1) + (TRational' n2 d2) = TRational' (  (n1* Positive2Int d1 :: TInt)
                                                               + (n2* Positive2Int d2 :: TInt)
                                                              )
                                                              (d1*d2)


