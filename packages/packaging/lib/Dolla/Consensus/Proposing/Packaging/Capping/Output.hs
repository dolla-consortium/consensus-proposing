module Dolla.Consensus.Proposing.Packaging.Capping.Output
  (Output (..))
  where

data Output a
  = Cut
  | Added a
  deriving (Show,Eq)