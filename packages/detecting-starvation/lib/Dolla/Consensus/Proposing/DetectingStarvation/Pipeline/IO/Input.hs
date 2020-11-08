{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia


data  Input
  = HandleLocalProposalConsumed
  | HandleLocalProposalProduced
  | HandleConsensusReached
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Input


