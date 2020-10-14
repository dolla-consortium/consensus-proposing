{-# LANGUAGE DeriveFunctor #-}

module  Dolla.Consensus.Proposing.Packaging.Serializing.Output
  (Output (..)) 
  where

data Output a
  = PipelineStarving
  | RequestSerialized a
  deriving (Eq,Show,Functor)

