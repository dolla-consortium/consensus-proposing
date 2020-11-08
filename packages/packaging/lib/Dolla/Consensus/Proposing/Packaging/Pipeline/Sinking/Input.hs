module Dolla.Consensus.Proposing.Packaging.Pipeline.Sinking.Input
  ( Input (..)) where

import Dolla.Common.Offset (Offset)


newtype Input = SinkNewLocalProposal {proposalId :: Offset}



