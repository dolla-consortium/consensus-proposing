{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Packaging.Serializing.Input
  ( Input (..)) where

data Input a
  = TransmitPipelineStarvingDownStream
  | RequestToSerialize a
 deriving (Functor,Show)

