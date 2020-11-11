module  Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Output
  (Output (..)) 
  where

newtype Output request = Output (Maybe request) deriving (Eq,Show)

