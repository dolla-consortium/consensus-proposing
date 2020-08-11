{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposer.Zeus.Local.MicroserviceSettings.ClientSimulator
  ( MicroserviceSettings (..))
  where

import           Prelude hiding (FilePath,writeFile)

import           Data.Aeson (encode)

import           Dolla.Common.NodeId (NodeId)
import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core (URL)

import           Dolla.Consensus.Common.Zeus.Logging
import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings

import           Dolla.Consensus.Proposer.Client.Simulator.Settings

import qualified Dolla.Consensus.Proposer.Receptionist.API.Client.Settings as Receptionist.Client


data MicroserviceSettings
  = MicroserviceSettings
  { nodeId :: NodeId
  , executableName :: String
  , logFileLocation :: FileSystemLocation
  , configurationLocation :: FileSystemLocation
  , receptionistUrl :: URL}

instance ExecutableSettingsProvider MicroserviceSettings  where
  getExecutableSettings clientSimulatorSettings @ MicroserviceSettings {executableName,logFileLocation,configurationLocation} =
    ExecutableSettings
    { executableName
    , executableSettings = encode $ getExecutableConfiguration clientSimulatorSettings
    , logFileLocation
    , configurationLocation}


getExecutableConfiguration :: MicroserviceSettings -> Settings
getExecutableConfiguration MicroserviceSettings {..}
  = Settings
    { logger = LoggerSettings { priority = INFO , loggerId = LoggerId "[cli-client-simulator]"}
    , receptionistClient
        = Receptionist.Client.Settings
          { logger = nodeLoggerId nodeId INFO  "receptionist.client"
          , nodeId
          , url = receptionistUrl }}

