{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dolla.Consensus.Proposing.Starving.Detecting.Execution.EventStore.Dependencies 
  (Dependencies (..)) where

import           Dolla.Common.NodeId
import           Dolla.Common.Dependencies.Core
import           Dolla.Common.Logging.Core

import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Consensus.Proposing.Starving.Detecting.Settings

data Dependencies
  = Dependencies
    { nodeId :: NodeId
    , logger :: Logger
    , eventStoreClient :: EventStore.Dependencies}


instance DependencyDerivable Settings Dependencies where
  acquireDependencies
    Settings
      { eventStoreClient = eventStoreClientSettings
      , logger = loggerSettings
      , nodeId
      }
    executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger ->
          acquireDependencies
                  eventStoreClientSettings
                  (\eventStoreClient -> executionUnderBracket Dependencies {..}))


instance HealthCheckable Dependencies where
  checkHealth d @ Dependencies {eventStoreClient}
    = (fmap . fmap) (const d) (checkHealth eventStoreClient)
