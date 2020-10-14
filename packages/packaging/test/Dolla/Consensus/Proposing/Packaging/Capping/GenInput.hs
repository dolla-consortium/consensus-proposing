module Dolla.Consensus.Proposing.Packaging.Capping.GenInput
  ( InputUnderTests (..)) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Packaging.Capping.Input


instance Arbitrary a => Arbitrary (InputUnderTests a) where
  arbitrary
    = InputUnderTests
      <$> oneof
          [ return AskForACut
          , Transmit <$> arbitrary]


newtype InputUnderTests a = InputUnderTests { unInputUnderTests :: Input a}
  deriving Show

