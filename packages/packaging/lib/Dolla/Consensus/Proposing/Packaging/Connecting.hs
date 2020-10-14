{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dolla.Consensus.Proposing.Packaging.Connecting
  () where

import           Data.Word (Word8)
import           Dolla.Adapter.Streamly.Connectable
import           Dolla.Consensus.Proposing.Packaging.Serializing.SerializedRequest

import qualified Dolla.Consensus.Proposing.Packaging.Input  as Packaging

import qualified Dolla.Consensus.Proposing.Packaging.Serializing.Input as Serializing
import qualified Dolla.Consensus.Proposing.Packaging.Serializing.Output as Serializing

import qualified Dolla.Consensus.Proposing.Packaging.NonEmptying.Input as NonEmptying
import qualified Dolla.Consensus.Proposing.Packaging.NonEmptying.Output as NonEmptying

import qualified Dolla.Consensus.Proposing.Packaging.Capping.Input as Capping
import qualified Dolla.Consensus.Proposing.Packaging.Capping.Output as Capping

import qualified Dolla.Consensus.Proposing.Packaging.Persisting.Input  as Persisting
import qualified Dolla.Consensus.Proposing.Packaging.Persisting.Output  as Persisting

import qualified Dolla.Consensus.Proposing.Packaging.Notifying.Input as Notifying

instance Connectable (Packaging.Input a) (Serializing.Input a) where
  connectIOs :: Packaging.Input a -> Serializing.Input a
  connectIOs
    = \case
      Packaging.PipelineStarving {} -> Serializing.TransmitPipelineStarvingDownStream
      Packaging.RequestData a -> Serializing.RequestToSerialize a

instance Connectable Persisting.Output  Notifying.Input where
  connectIOs
    = \case
      Persisting.LocalProposalPersisted {..} -> Notifying.LocalProposalProduced {..}

instance Connectable (Serializing.Output a) (NonEmptying.Input a) where
  connectIOs
    = \case
      Serializing.PipelineStarving  -> Nothing
      Serializing.RequestSerialized a -> Just a

instance Connectable (NonEmptying.Output a) (Capping.Input a) where
  connectIOs
    = \case
      Nothing  -> Capping.AskForACut
      Just a -> Capping.Transmit a

instance Connectable (Capping.Output SerializedRequest) (Persisting.Input [Word8]) where
  connectIOs
    = \case
      Capping.Cut -> Persisting.CommitProposal
      Capping.Added (SerializedRequest word8s) -> Persisting.PersistRequest word8s
