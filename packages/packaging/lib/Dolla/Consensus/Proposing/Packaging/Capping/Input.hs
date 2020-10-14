{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Packaging.Capping.Input
  ( Input (..)) where

data Input a
  = AskForACut
  | Transmit a
  deriving (Eq,Show,Functor)

