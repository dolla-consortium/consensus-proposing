{-# LANGUAGE NamedFieldPuns
           , DerivingVia
           , FlexibleContexts
           , UndecidableInstances
           , DuplicateRecordFields
           , RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Simulating.Dependencies (Dependencies (..)) where

import           Dolla.Common.Logging.Core
import           Dolla.Common.Dependencies.Core
import qualified Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Client.Dependencies as Receptionist.Client
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Consensus.Proposing.Simulating.Settings
import           Dolla.Consensus.Proposing.Simulating.StressLoad

data Dependencies
  = Dependencies
    { logger :: Logger
    , receptioningClient :: Receptionist.Client.Dependencies
    , eventStoreClient :: EventStore.Dependencies
    , stressLoad :: StressLoad}

instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {logger = loggerSettings, eventStoreClient = eventStoreClientSettings , receptioningClient = receptioningClientSettings,stressLoad} executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger ->
            acquireDependencies
              receptioningClientSettings
              (\receptioningClient ->
                  acquireDependencies
                    eventStoreClientSettings
                    (\eventStoreClient -> executionUnderBracket Dependencies {..})))

instance HealthCheckable Dependencies where
  checkHealth d @ Dependencies {receptioningClient}
    = do
      receptioningClientsHealth <- checkHealth receptioningClient
      (fmap . fmap) (const d)
        $ return $ () <$ receptioningClientsHealth