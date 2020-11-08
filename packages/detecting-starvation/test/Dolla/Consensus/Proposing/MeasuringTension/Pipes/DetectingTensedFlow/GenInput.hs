{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.GenInput
  ( ConsistentInputs (..)
  , InputUnderTests (..)) where

import Data.Coerce (coerce)
import Data.Monoid

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Input

instance Arbitrary InputUnderTests where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return Staged
          , return Released
          , return Pulled]

newtype InputUnderTests = InputUnderTests { unInputUnderTests :: Input} deriving Show

newtype ConsistentInputs = ConsistentInputs { unConsistentInputs :: [InputUnderTests]} deriving Show

instance Arbitrary ConsistentInputs where
  arbitrary
    = suchThat
      (ConsistentInputs <$> listOf  arbitrary)
      (\inputs -> itemInTransit (coerce inputs) > 0)

itemInTransit :: [Input] -> Sum Integer
itemInTransit
  = foldMap
      (\case
         Released -> - 1
         Staged -> 1
         Pulled -> 0)

