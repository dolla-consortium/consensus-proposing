{-# LANGUAGE NamedFieldPuns
           , DerivingVia
           , FlexibleContexts
           , UndecidableInstances
           , DuplicateRecordFields
           , RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Requesting.Simulating.Dependencies (Dependencies (..)) where

import           Dolla.Common.Logging.Core
import           Dolla.Common.Dependencies.Core
import qualified Dolla.Consensus.Proposing.Receptioning.API.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposing.Client.Simulator.Settings

data Dependencies
  = Dependencies
    { logger :: Logger
    , receptioningClient :: Receptionist.Client.Dependencies }

instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {logger = loggerSettings, receptioningClient = receptioningClientSettings} executionUnderBracket
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