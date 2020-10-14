module Dolla.Consensus.Proposing.Packaging.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Packaging.Input
import Dolla.Common.Offset

instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ PipelineStarving <$> arbitrary
          , RequestData <$> arbitrary]


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a} deriving Show

