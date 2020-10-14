{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Packaging.Persisting.Input
  (Input (..)) 
  where

data Input a
  = CommitProposal
  | PersistRequest a
  deriving (Show,Eq,Functor)