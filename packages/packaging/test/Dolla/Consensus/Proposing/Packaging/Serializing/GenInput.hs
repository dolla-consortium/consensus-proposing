module Dolla.Consensus.Proposing.Packaging.Serializing.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Packaging.Serializing.Input


instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return TransmitPipelineStarvingDownStream
          , RequestToSerialize <$> arbitrary]


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a}
  deriving Show

