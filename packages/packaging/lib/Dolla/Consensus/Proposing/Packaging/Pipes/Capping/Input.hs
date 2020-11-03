{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Input
  ( Input (..)) where

data Input request
  = AskForACut
  | Add request
  deriving (Eq,Show,Functor)

