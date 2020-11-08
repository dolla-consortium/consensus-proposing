module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.GenOutput
  ( OutputUnderTests (..)) where

import Test.QuickCheck.Arbitrary

import Test.QuickCheck.Instances.UUID ()

import Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Output

instance Arbitrary OutputUnderTests where
  arbitrary = OutputUnderTests . NewLocalProposalAsked <$> arbitrary

newtype OutputUnderTests = OutputUnderTests { unOutputUnderTests :: Output} deriving Show

