module Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.IO.GenOutput
  ( OutputUnderTests (..)) where

import Test.QuickCheck.Arbitrary

import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.IO.Output

instance Arbitrary OutputUnderTests where
  arbitrary = OutputUnderTests . LocalProposalStarvationDetected <$> arbitrary

newtype OutputUnderTests = OutputUnderTests { unOutputUnderTests :: Output} deriving Show

