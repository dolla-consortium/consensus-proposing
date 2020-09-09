{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}
module Dolla.Consensus.Proposing.Flushing.Input (Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Consensus.Log.Aggregation

data  Input
  = ProposalAccepted { byProposer    :: ByProposer}
  | LocalProposalProduced
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Input

