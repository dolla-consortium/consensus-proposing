{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dolla.Consensus.Proposing.Packaging.Dependencies (Dependencies (..)) where

import           Dolla.Common.NodeId
import           Dolla.Common.Dependencies.Core
import           Dolla.Common.Logging.Core
import           Dolla.Common.Memory.Byte (Byte)

import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Consensus.Proposal.Persistence
import           Dolla.Consensus.Proposing.Packaging.Settings

data Dependencies
  = Dependencies
    { nodeId :: NodeId
    , logger :: Logger
    , proposalRootFolder :: ProposalRootFolder
    , proposalSizeLimit :: Byte
    , eventStoreClient :: EventStore.Dependencies}


instance DependencyDerivable Settings Dependencies where
  acquireDependencies
    Settings
      { eventStoreClient = eventStoreClientSettings
      , logger = loggerSettings
      , proposalRootFolder
      , proposalSizeLimit
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
