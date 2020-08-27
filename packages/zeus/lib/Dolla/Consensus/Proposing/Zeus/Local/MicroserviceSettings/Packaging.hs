{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.Packaging
  ( MicroServiceSettings (..))
  where

import           Prelude hiding (writeFile)



import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
import           Dolla.Consensus.Proposing.Packaging.Settings
import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core
import           Dolla.Consensus.EventStore.Zeus.Local.Settings
import           Dolla.Consensus.Common.Zeus.Logging

import Data.Aeson (encode)

data MicroServiceSettings
  = MicroServiceSettings
  { nodeId :: NodeId
  , executableName :: String
  , logFileLocation :: FileSystemLocation
  , configurationLocation :: FileSystemLocation
  , eventStore :: EventStoreSettings
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
     , logger = nodeLoggerId nodeId DEBUG "proposer"
     , eventStoreClient = mapToEventStoreSettings eventStore $ nodeLoggerId nodeId DEBUG  "event.store.client"}

