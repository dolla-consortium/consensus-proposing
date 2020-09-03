
module Dolla.Consensus.Proposing.Receptioning.Service.OverEventStore
  ( persistClientRequest
  , persistClientRequests) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Dummy.Client.Request

import qualified Dolla.Consensus.Proposing.Receptioning.Service.Generic as Service.Generic
import           Dolla.Consensus.Log.EventStoreLog

persistClientRequest
  :: EventStoreClient.Dependencies
  -> ClientRequest
  -> IO ()
persistClientRequest eventStoreClientDependencies =
  Service.Generic.transmitClientRequestToProposing
    (getEventStoreLog eventStoreClientDependencies LocalRequestLog)

persistClientRequests
  :: EventStoreClient.Dependencies
  -> NonEmpty ClientRequest
  -> IO ()
persistClientRequests eventStoreClientDependencies =
  Service.Generic.transmitClientRequestsToProposing
    (getEventStoreLog eventStoreClientDependencies LocalRequestLog)