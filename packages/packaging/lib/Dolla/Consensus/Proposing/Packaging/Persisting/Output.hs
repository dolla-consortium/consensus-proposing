module Dolla.Consensus.Proposing.Packaging.Persisting.Output
  (Output (..)) 
  where

import Dolla.Common.Offset (Offset)

newtype Output = LocalProposalPersisted {proposalId :: Offset} deriving (Show,Eq)
