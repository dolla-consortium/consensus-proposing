{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Starving.Detecting.Output
  (Output (..)) where

import           Data.Aeson
import           Data.UUID (UUID)
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

newtype Output
  = PipelineStarving {itemId :: UUID}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Output

instance Appendable Output where
  getItemName (PipelineStarving _ )  = "PipelineStarving"

instance UUIDProvider Output where
  getUUID PipelineStarving {itemId} = itemId

