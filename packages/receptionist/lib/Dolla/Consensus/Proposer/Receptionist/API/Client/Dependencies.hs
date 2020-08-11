{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dolla.Consensus.Proposer.Receptionist.API.Client.Dependencies (Dependencies (..)) where

import           Dolla.Common.Dependencies.Core

import           Servant.Client
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Dolla.Common.NodeId
import           Dolla.Consensus.Proposer.Receptionist.API.Client.Client
import           Dolla.Common.Logging.Core
import           Dolla.Adapter.Servant.Adapter
import           Dolla.Consensus.Proposer.Receptionist.API.Client.Settings

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
