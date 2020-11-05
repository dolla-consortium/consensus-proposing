{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia


data  Input
  = LocalProposalConsumed
  | LocalProposalProduced
  | LocalProposalAsked
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Input


