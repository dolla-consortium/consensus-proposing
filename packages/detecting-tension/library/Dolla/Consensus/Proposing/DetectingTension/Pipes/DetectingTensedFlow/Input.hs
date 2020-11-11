{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

data  Input
  = Staged
  | Released
  | Pulled
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Input


