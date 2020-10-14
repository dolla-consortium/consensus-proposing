{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Packaging.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Data.UUID (UUID)

data Input a
  = PipelineStarving {itemId :: UUID }
  | RequestData a
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON (Input a)
  deriving Functor

instance Appendable a => Appendable (Input a) where
  getItemName (RequestData a)  = "RequestData." ++ getItemName a
  getItemName (PipelineStarving _ )  = "PipelineStarving"


instance UUIDProvider a => UUIDProvider (Input a) where
  getUUID (RequestData a) = getUUID a
  getUUID PipelineStarving {itemId} = itemId

