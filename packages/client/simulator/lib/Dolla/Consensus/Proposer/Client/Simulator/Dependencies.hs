{-# LANGUAGE NamedFieldPuns
           , DerivingVia
           , FlexibleContexts
           , UndecidableInstances
           , DuplicateRecordFields
           , RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposer.Client.Simulator.Dependencies (Dependencies (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Logging.Core
import           Dolla.Common.Dependencies.Core
import qualified Dolla.Consensus.Proposer.Receptionist.API.Client.Settings as Receptionist.Client
import qualified Dolla.Consensus.Proposer.Receptionist.API.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposer.Client.Simulator.Settings

data Dependencies
  = Dependencies
    { logger :: Logger
    , receptionistClient :: Receptionist.Client.Dependencies }

instance DependencyDerivable Settings Dependencies where
  acquireDependencies Settings {logger = loggerSettings, receptionistClient = receptionistClientSettings} executionUnderBracket
    = acquireDependencies
        loggerSettings
        (\logger ->
            acquireDependencies
              receptionistClientSettings
              (\receptionistClient -> executionUnderBracket Dependencies {..}))

instance HealthCheckable Dependencies where
  checkHealth d @ Dependencies {receptionistClient}
    = do
      receptionistClientsHealth <- checkHealth receptionistClient
      (fmap . fmap) (const d)
        $ return $ () <$ receptionistClientsHealth