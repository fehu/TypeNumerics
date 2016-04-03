-----------------------------------------------------------------------------
--
-- Module      :  TypeNum.TTest.Common
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE GADTs, PolyKinds #-}


module TypeNum.TTest.Common(

  correct, mistake
, B(..)

, module TypeNum
, module Test.Hspec


) where


import TypeNum

import Test.Hspec

-----------------------------------------------------------------------------

data B (b :: Bool) = B


correct :: (expr ~ True) => c expr -> Bool
correct _ = True

mistake :: (expr ~ False) => c expr -> Bool
mistake _ = True

