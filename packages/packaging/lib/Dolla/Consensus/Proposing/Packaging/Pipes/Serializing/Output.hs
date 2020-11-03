{-# LANGUAGE DeriveFunctor #-}

module  Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Output
  (Output (..)) 
  where

data Output a
  = ProposalProductionNotForced
  | Serialized a
  deriving (Eq,Show,Functor)

