module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.GenOutput
  ( OutputUnderTests (..)) where

import Test.QuickCheck.Arbitrary

import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Output

instance Arbitrary OutputUnderTests where
  arbitrary = OutputUnderTests . LocalProposalStarvationDetected <$> arbitrary

newtype OutputUnderTests = OutputUnderTests { unOutputUnderTests :: Output} deriving Show

