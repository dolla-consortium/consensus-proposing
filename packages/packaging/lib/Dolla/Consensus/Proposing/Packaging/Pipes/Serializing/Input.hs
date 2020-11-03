{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Input
  ( Input (..)) where

data Input request
  = ForceProposalProduction
  | Serialize request
 deriving (Functor,Show)

