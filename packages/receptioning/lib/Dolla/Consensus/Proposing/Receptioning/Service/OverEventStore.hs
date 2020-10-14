
module Dolla.Consensus.Proposing.Receptioning.Service.OverEventStore
  ( transmitRequestToProposingPackagingPipeline
  , transmitRequestsToProposingPackagingPipeline) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Request

import qualified Dolla.Consensus.Proposing.Receptioning.Service.Generic as Service.Generic
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Control.Monad.IO.Class

transmitRequestToProposingPackagingPipeline
  :: ( MonadIO m
     , Appendable clientRequest
     , Appendable consortiumRequest
     , UUIDProvider clientRequest
     , UUIDProvider consortiumRequest)
  => EventStoreClient.Dependencies
  -> Request clientRequest consortiumRequest
  -> m ()
transmitRequestToProposingPackagingPipeline eventStoreClientDependencies =
  Service.Generic.transmitRequestToProposingPackagingPipeline
    (getEventStoreLog 
      eventStoreClientDependencies 
      LocalRequestLog)

transmitRequestsToProposingPackagingPipeline
  :: ( MonadIO m
     , Show clientRequest
     , Show consortiumRequest
     , Appendable clientRequest
     , Appendable consortiumRequest
     , UUIDProvider clientRequest
     , UUIDProvider consortiumRequest)
  => EventStoreClient.Dependencies
  -> NonEmpty (Request clientRequest consortiumRequest)
  -> m ()
transmitRequestsToProposingPackagingPipeline eventStoreClientDependencies =
  Service.Generic.transmitRequestsToProposingPackagingPipeline
    (getEventStoreLog 
      eventStoreClientDependencies 
      LocalRequestLog)