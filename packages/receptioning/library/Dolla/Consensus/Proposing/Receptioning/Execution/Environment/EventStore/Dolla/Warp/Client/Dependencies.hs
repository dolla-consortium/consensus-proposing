{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Client.Dependencies
  (Dependencies (..)) where


import           Servant.Client

import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import           Dolla.Adapter.Servant.Adapter

import           Dolla.Common.NodeId
import           Dolla.Common.Dependencies.Core
import           Dolla.Common.Logging.Core


import           Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Client.Settings
import           Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Client.Client

data Dependencies 
  = Dependencies 
    { logger :: Logger 
    , nodeId :: NodeId
    , url :: BaseUrl
    , httpClientManager :: Manager}

instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {nodeId,url, logger = loggerSettings} executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger -> do
          httpClientManager <- newManager defaultManagerSettings
          executionUnderBracket Dependencies {url = toBaseUrl url ,..})


instance HealthCheckable Dependencies where
  checkHealth dependencies @ Dependencies {httpClientManager,url}
    = (fmap . fmap)
      (const dependencies)
      (sendHealthCheckRequest httpClientManager url)
