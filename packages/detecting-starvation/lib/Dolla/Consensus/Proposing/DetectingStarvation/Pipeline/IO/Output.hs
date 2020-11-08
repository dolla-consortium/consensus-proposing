{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Output
  (Output (..)) where

import           Data.Aeson
import           Data.UUID (UUID)
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

newtype Output
  = NewLocalProposalAsked  {itemId :: UUID}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Output

instance Appendable Output where
  getItemName (NewLocalProposalAsked _ )  = "NewLocalProposalAsked"

instance UUIDProvider Output where
  getUUID NewLocalProposalAsked {itemId} = itemId

