module Dolla.Consensus.Proposing.Staging.Pipes.Serializing.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Input


instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary = InputUnderTests <$> arbitrary


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a}
  deriving Show

