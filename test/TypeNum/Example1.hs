-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.Example1
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Main where

import TypeNum.Integer
import TypeNum.Rational


class I a where implicitly :: a


data A = A deriving Show
instance I A where implicitly = A



data (I a) => D a (b :: TInt) = D a (Int' b) deriving Show
instance (I a) => I (D a b) where implicitly = D implicitly Int'


data (I a) => R a (b :: TRational) = R a (Ratio' b)
instance (I a, Show a, KnownRatio b) =>
    Show (R a b) where show (R a b) = "R " ++ show a ++ " " ++ show b

instance (I a) => I (R a b) where implicitly = R implicitly Ratio'

main = do print (implicitly :: D A (Pos 4))
          print (implicitly :: R A (Pos 4 :% 1))
          print (implicitly :: R A (Pos 4 :% 3))
