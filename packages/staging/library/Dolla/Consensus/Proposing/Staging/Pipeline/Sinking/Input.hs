module Dolla.Consensus.Proposing.Staging.Pipeline.Sinking.Input
  ( Input (..)) where

import Dolla.Common.Offset (Offset)


newtype Input = SinkNewLocalProposal {proposalId :: Offset}



