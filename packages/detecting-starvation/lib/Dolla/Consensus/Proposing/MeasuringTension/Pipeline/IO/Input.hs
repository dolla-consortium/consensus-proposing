{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.MeasuringTension.Pipeline.IO.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia


data  Input
  = LocalProposalAccepted
  | LocalProposalStaged
  | ConsensusReached
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Input


