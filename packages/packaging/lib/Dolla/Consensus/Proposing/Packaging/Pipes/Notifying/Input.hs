module Dolla.Consensus.Proposing.Packaging.Pipes.Notifying.Input
  ( Input (..)) where

import Dolla.Common.Offset (Offset)


newtype Input = LocalProposalProduced {proposalId :: Offset}



