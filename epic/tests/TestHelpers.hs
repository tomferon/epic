{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestHelpers
  ( module TestHelpers
  , module Test.Tasty
  , module Test.Tasty.Hspec
  , module Test.Tasty.QuickCheck
  ) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Epic.Language

instance Arbitrary Type where
  arbitrary = oneof
    [ TypeVariable <$> arbitrary
    , FunctionType <$> arbitrary <*> arbitrary
    , UniversalType <$> arbitrary
    , return PrimTypeInt
    ]
