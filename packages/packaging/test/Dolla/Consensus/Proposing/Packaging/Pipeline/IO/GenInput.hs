module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()

import Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input


instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return ForceProposalProduction
          , Package <$> arbitrary]


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a} deriving Show

