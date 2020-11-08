{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Staging.Pipes.Persisting.Input
  (Input (..)) 
  where

data Input request
  = CommitProposal
  | Persist request
  deriving (Show,Eq,Functor)