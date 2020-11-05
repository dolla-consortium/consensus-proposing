module Dolla.Consensus.Proposing.Packaging.Pipes.Appending.Input
  ( Input (..)) where

import Dolla.Common.Offset (Offset)


newtype Input = LocalProposalProduced {proposalId :: Offset}



