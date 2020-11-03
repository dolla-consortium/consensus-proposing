{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Dolla.Consensus.Proposing.Receptioning.Output
  ( Output (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider

newtype Output request
  = Receptioned request 
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON (Output request)
  deriving Functor


instance Appendable a => Appendable (Output a) where
  getItemName (Receptioned request)  = "Receptioned." ++ getItemName request

instance UUIDProvider a => UUIDProvider (Output a) where
  getUUID (Receptioned request) = getUUID request

