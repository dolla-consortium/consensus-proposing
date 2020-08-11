{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposer.Zeus.Local.MicroserviceSettings.Receptionist
  ( MicroServiceSettings (..))
  where

import           Prelude hiding (writeFile)
import           Data.Aeson (encode)

import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core

import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
import           Dolla.Consensus.Common.Zeus.Logging
import           Dolla.Consensus.EventStore.Zeus.Local.Settings
import qualified Dolla.Consensus.Proposer.Receptionist.API.Server.Settings   as Receptionist

data MicroServiceSettings
  = MicroServiceSettings
  { nodeId :: NodeId
  , executableName :: String
  , logFileLocation :: FileSystemLocation
  , configurationLocation :: FileSystemLocation
  , eventStore :: EventStoreSettings
  , receptionistUrl :: URL}

instance ExecutableSettingsProvider MicroServiceSettings  where
  getExecutableSettings broadcastSettings @ MicroServiceSettings {executableName,logFileLocation,configurationLocation} =
    ExecutableSettings
    { executableName
    , executableSettings = encode $ getExecutableConfiguration broadcastSettings
    , logFileLocation
    , configurationLocation}
   
getExecutableConfiguration :: MicroServiceSettings -> Receptionist.Settings
getExecutableConfiguration MicroServiceSettings {..}
  = Receptionist.Settings
    { nodeId
    , logger = nodeLoggerId nodeId INFO  "receptionist"
    , healthCheckLogger = nodeLoggerId nodeId INFO  "healthcheck"
    , eventStoreClientSettings = mapToEventStoreSettings eventStore $ nodeLoggerId nodeId INFO  "event.store.client"
    , url = receptionistUrl }




