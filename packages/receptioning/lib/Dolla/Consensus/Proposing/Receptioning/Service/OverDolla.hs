
module Dolla.Consensus.Proposing.Receptioning.Service.OverDolla
  ( persistClientRequest
  , persistClientRequests) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Consensus.Consortium.Request
import qualified Dolla.Consensus.Proposing.Receptioning.Service.OverEventStore as OverEventStore
import           Dolla.Consensus.Request
import           Control.Monad.IO.Class 


persistClientRequest
  :: MonadIO m
  => EventStoreClient.Dependencies
  -> DollaClientRequest
  -> m ()
persistClientRequest eventStoreClientDependencies clientRequest =
  OverEventStore.transmitRequestToProposingPackagingPipeline
    eventStoreClientDependencies
    (ClientReq clientRequest :: Request DollaClientRequest ConsortiumRequest)

persistClientRequests
  :: MonadIO m
  => EventStoreClient.Dependencies
  -> NonEmpty DollaClientRequest
  -> m ()
persistClientRequests eventStoreClientDependencies clientRequests =
  OverEventStore.transmitRequestsToProposingPackagingPipeline
    eventStoreClientDependencies
    ((ClientReq <$> clientRequests) :: NonEmpty (Request DollaClientRequest ConsortiumRequest))
