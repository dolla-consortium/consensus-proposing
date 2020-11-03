module Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Output
  (Output (..))
  where

data Output request
  = Cut
  | Added request
  deriving (Show,Eq)