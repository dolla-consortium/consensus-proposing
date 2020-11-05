{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.GenInput
  ( ConsistentInputs (..)
  , InputUnderTests (..)) where

import Data.Coerce (coerce)
import Data.Monoid

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Input

instance Arbitrary InputUnderTests where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return LocalProposalConsumed
          , return LocalProposalProduced
          , return LocalProposalAsked]

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
         LocalProposalConsumed -> - 1
         LocalProposalProduced -> 1
         LocalProposalAsked -> 0)

