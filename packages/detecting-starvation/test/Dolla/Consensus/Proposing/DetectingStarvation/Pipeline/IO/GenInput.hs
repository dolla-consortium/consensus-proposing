{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.GenInput
  ( ConsistentInputs (..)
  , InputUnderTests (..)) where

import Data.Coerce (coerce)
import Data.Monoid

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input

instance Arbitrary InputUnderTests where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return HandleLocalProposalConsumed
          , return HandleLocalProposalProduced
          , return HandleConsensusReached]

newtype InputUnderTests = InputUnderTests { unInputUnderTests :: Input} deriving Show

newtype ConsistentInputs = ConsistentInputs { unConsistentInputs :: [InputUnderTests]} deriving Show

instance Arbitrary ConsistentInputs where
  arbitrary
    = suchThat
      (ConsistentInputs <$> listOf  arbitrary)
      (\inputs -> delta (coerce inputs) > 0)

delta :: [Input] -> Sum Integer
delta
  = foldMap
      (\case
         HandleLocalProposalConsumed -> - 1
         HandleLocalProposalProduced -> 1
         HandleConsensusReached -> 0)

