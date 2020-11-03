module Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Output
  (Output (..)) 
  where

import Dolla.Common.Offset (Offset)

newtype Output = LocalProposalPersisted {proposalId :: Offset} deriving (Show,Eq)
