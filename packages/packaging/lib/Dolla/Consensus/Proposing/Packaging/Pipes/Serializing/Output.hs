module  Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Output
  (Output (..)) 
  where

newtype Output request = Output (Maybe request) deriving (Eq,Show)

