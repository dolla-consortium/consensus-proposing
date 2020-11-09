{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dolla.Consensus.Proposing.Staging.Pipeline.Welding.BluePrint
  () where

import           Data.Word (Word8)
import           Data.Coerce (coerce)
import           Dolla.Common.Pipeline.Weldable
import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.SerializedRequest

import qualified Dolla.Consensus.Proposing.Staging.Pipeline.IO.Input  as Staging

import qualified Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Input as Serializing
import qualified Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Output as Serializing

import qualified Dolla.Consensus.Proposing.Staging.Pipes.NonEmptying.Input as NonEmptying
import qualified Dolla.Consensus.Proposing.Staging.Pipes.NonEmptying.Output as NonEmptying

import qualified Dolla.Consensus.Proposing.Staging.Pipes.Capping.Input as Capping
import qualified Dolla.Consensus.Proposing.Staging.Pipes.Capping.Output as Capping

import qualified Dolla.Consensus.Proposing.Staging.Pipes.Persisting.Input  as Persisting
import qualified Dolla.Consensus.Proposing.Staging.Pipes.Persisting.Output  as Persisting

import qualified Dolla.Consensus.Proposing.Staging.Pipeline.Sinking.Input as Sinking

instance Weldable (Staging.Input request) (Serializing.Input request) where
  weld
    = \case
      Staging.Stage {} -> Nothing
      Staging.Package request -> Just request

instance Weldable (Serializing.Output a) (NonEmptying.Input a) where
  weld = coerce

instance Weldable (NonEmptying.Output a) (Capping.Input a) where
  weld
    = \case
      Nothing  -> Capping.AskForACut
      Just a -> Capping.Add a

instance Weldable (Capping.Output SerializedRequest) (Persisting.Input [Word8]) where
  weld
    = \case
      Capping.Cut -> Persisting.CommitProposal
      Capping.Added (SerializedRequest word8s) -> Persisting.Persist word8s

instance Weldable Persisting.Output  Sinking.Input where
  weld
    = \case
      Persisting.LocalProposalPersisted {..} -> Sinking.SinkNewLocalProposal {..}
