{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposer.Receptionist.API.Server.Settings (Settings (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia


import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core

import           Dolla.Common.NodeId

data Settings = Settings {
                  nodeId :: NodeId,
                  logger :: LoggerSettings,
                  healthCheckLogger :: LoggerSettings,
                  url :: URL,
                  eventStoreClientSettings :: EventStore.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

