{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dolla.Consensus.Proposing.Receptioning.API.Server.Dependencies (Dependencies (..)) where


import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core
import           Dolla.Common.Dependencies.Core
import           Dolla.Common.NodeId
import           Dolla.Consensus.Proposing.Receptioning.API.Server.Settings

data Dependencies = Dependencies{
                        nodeId :: NodeId,
                        logger :: Logger,
                        url :: URL,
                        eventStoreClientDependencies :: EventStore.Dependencies}


instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {nodeId,logger = loggerSettings, eventStoreClientSettings, url} executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger ->
            acquireDependencies
              eventStoreClientSettings
              (\eventStoreClientDependencies -> executionUnderBracket Dependencies {..}))


instance HealthCheckable Dependencies where
  checkHealth d @ Dependencies {eventStoreClientDependencies}
    = (fmap . fmap) (const d) (checkHealth eventStoreClientDependencies)