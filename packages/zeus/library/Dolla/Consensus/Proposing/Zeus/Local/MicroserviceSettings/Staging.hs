{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.Staging
  ( MicroServiceSettings (..))
  where

import           Prelude hiding (writeFile)

import           Data.Aeson (encode)


import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core

import           Dolla.Consensus.EventStore.Zeus.Local.Settings
import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
import           Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Settings
import           Dolla.Consensus.Common.Zeus.Logging

data MicroServiceSettings
  = MicroServiceSettings
  { nodeId :: NodeId
  , executableName :: String
  , logFileLocation :: FileSystemLocation
  , configurationLocation :: FileSystemLocation
  , eventStore :: EventStoreSettings
  , proposalSizeLimit :: Byte
  , proposalRootFolder :: FilePath}

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
     , proposalRootFolder
     , proposalSizeLimit
     , logger = nodeLoggerId nodeId DEBUG "proposer"
     , eventStoreClient = mapToEventStoreSettings eventStore $ nodeLoggerId nodeId DEBUG  "event.store.client"}

