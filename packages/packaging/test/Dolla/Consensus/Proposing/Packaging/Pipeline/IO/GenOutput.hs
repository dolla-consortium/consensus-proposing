module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.GenOutput
  ( OutputUnderTests (..)
  ) where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.UUID ()
import Test.QuickCheck.Instances ()
import Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output
import Dolla.Common.Offset

newtype OutputUnderTests = OutputUnderTests { unOutputUnderTests :: Output} deriving Show

instance Arbitrary OutputUnderTests where
  arbitrary = OutputUnderTests . LocalProposalProduced . Offset <$> arbitrary

