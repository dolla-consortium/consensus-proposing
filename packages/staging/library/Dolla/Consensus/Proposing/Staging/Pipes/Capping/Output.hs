module Dolla.Consensus.Proposing.Staging.Pipes.Capping.Output
  (Output (..))
  where

data Output request
  = Cut
  | Added request
  deriving (Show,Eq)