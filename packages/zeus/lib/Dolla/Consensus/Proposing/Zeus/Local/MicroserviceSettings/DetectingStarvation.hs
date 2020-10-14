{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.DetectingStarvation
  ( MicroServiceSettings (..))
  where

import           Prelude hiding (writeFile)

import           Data.Aeson (encode)

import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core

import           Dolla.Consensus.EventStore.Zeus.Local.Settings
import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
import           Dolla.Consensus.Proposing.Starving.Detecting.Settings
import           Dolla.Consensus.Common.Zeus.Logging

data MicroServiceSettings
  = MicroServiceSettings
  { nodeId :: NodeId
  , executableName :: String
  , logFileLocation :: FileSystemLocation
  , configurationLocation :: FileSystemLocation
  , eventStore :: EventStoreSettings}

instance ExecutableSettingsProvider MicroServiceSettings  where
  getExecutableSettings broadcastSettings @ MicroServiceSettings {executableName,logFileLocation, configurationLocation} =
    ExecutableSettings
    { executableName
    , executableSettings = encode $ getConfiguration broadcastSettings
    , logFileLocation
    , configurationLocation}
   
getConfiguration :: MicroServiceSettings -> Settings
getConfiguration MicroServiceSettings {..}
  = Settings
     { nodeId
     , logger = nodeLoggerId nodeId DEBUG "starving-detection"
     , eventStoreClient = mapToEventStoreSettings eventStore $ nodeLoggerId nodeId DEBUG  "event.store.client"}

