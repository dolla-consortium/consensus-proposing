
module Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Service
  ( receptioning) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Consensus.Consortium.Request
import qualified Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Service as OverEventStore
import           Dolla.Consensus.Request
import           Control.Monad.IO.Class 


receptioning
  :: MonadIO m
  => EventStoreClient.Dependencies
  -> NonEmpty DollaClientRequest
  -> m ()
receptioning eventStoreClient clientRequests =
  OverEventStore.receptioning
    eventStoreClient
    ((ClientReq <$> clientRequests) :: NonEmpty (Request DollaClientRequest ConsortiumRequest))
