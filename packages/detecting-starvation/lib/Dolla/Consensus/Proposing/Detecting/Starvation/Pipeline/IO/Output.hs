{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.IO.Output
  (Output (..)) where

import           Data.Aeson
import           Data.UUID (UUID)
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

newtype Output
  = LocalProposalStarvationDetected  {itemId :: UUID}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Output

instance Appendable Output where
  getItemName (LocalProposalStarvationDetected _ )  = "LocalProposalStarvationDetected"

instance UUIDProvider Output where
  getUUID LocalProposalStarvationDetected {itemId} = itemId

