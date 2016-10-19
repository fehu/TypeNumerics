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

{-# LANGUAGE FlexibleContexts, PolyKinds #-}

module Main where

import TypeNum.Rational

-----------------------------------------------------------------------------


class I a where implicitly :: a


data A = A deriving Show
instance I A where implicitly = A

main = undefined

-----------------------------------------------------------------------------

data D a (b :: TInt) = D a (Int' b) deriving Show
instance (I a) => I (D a b) where implicitly = D implicitly Int'

-----------------------------------------------------------------------------

data R a (b :: TRational) = R a (Ratio' b)
instance (I a, Show a, KnownRatio b) =>
    Show (R a b) where show (R a b) = "R " ++ show a ++ " " ++ show b

instance (I a) => I (R a b) where implicitly = R implicitly Ratio'

-----------------------------------------------------------------------------

type R' a (b :: k) = R a (AsRational b)

-----------------------------------------------------------------------------

main = do print (implicitly :: D A (Pos 4))
          print (implicitly :: R A (Pos 4 :% 1))
          print (implicitly :: R A (Pos 4 :% 3))
          putStrLn ""
          print (implicitly :: R' A (Pos 4 :% 3))
          print (implicitly :: R' A 2)
          print (implicitly :: R' A (Neg 1))
