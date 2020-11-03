module Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Input


instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return CommitProposal
          , Persist <$> arbitrary]


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a}
  deriving Show

