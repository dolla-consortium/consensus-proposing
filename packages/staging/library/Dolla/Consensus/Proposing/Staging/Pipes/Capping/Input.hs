{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Staging.Pipes.Capping.Input
  ( Input (..)) where

data Input request
  = AskForACut
  | Add request
  deriving (Eq,Show,Functor)

