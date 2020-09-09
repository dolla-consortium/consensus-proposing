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
import qualified Dolla.Consensus.Proposing.Receptioning.API.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposing.Simulating.Settings
import           Dolla.Consensus.Proposing.Simulating.StressLoad

data Dependencies
  = Dependencies
    { logger :: Logger
    , receptioningClient :: Receptionist.Client.Dependencies
    , stressLoad :: StressLoad}

instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {logger = loggerSettings, receptioningClient = receptioningClientSettings,stressLoad} executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger ->
            acquireDependencies
              receptioningClientSettings
              (\receptioningClient -> executionUnderBracket Dependencies {..}))

instance HealthCheckable Dependencies where
  checkHealth d @ Dependencies {receptioningClient}
    = do
      receptioningClientsHealth <- checkHealth receptioningClient
      (fmap . fmap) (const d)
        $ return $ () <$ receptioningClientsHealth