{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestHelpers
  ( module TestHelpers
  , module Hedgehog
  , module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.Hedgehog
  ) where

import qualified Data.Text as T

import           Hedgehog hiding (assert)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Epic.Language
import           Epic.Parser.Internal

identifierGen :: Gen T.Text
identifierGen = Gen.filter (`notElem` reservedKeywords) $ do
  fc   <- Gen.lower
  rest <- Gen.text (Range.linear 0 10) $
    Gen.frequency [(9, Gen.alphaNum), (1, return '_')]
  return $ T.cons fc rest

constructorGen :: Gen T.Text
constructorGen = Gen.filter (`notElem` ["Bool", "Int"]) $ do
  fc   <- Gen.upper
  rest <- Gen.text (Range.linear 0 10) $
    Gen.frequency [(9, Gen.alphaNum), (1, return '_')]
  return $ T.cons fc rest

moduleNameGen :: Gen ModuleName
moduleNameGen =
    ModuleName <$> Gen.list (Range.constant 1 5) partsGen
  where
    partsGen :: Gen T.Text
    partsGen = do
      fc   <- Gen.upper
      rest <- Gen.text (Range.linear 0 10) Gen.alpha
      return $ T.cons fc rest
