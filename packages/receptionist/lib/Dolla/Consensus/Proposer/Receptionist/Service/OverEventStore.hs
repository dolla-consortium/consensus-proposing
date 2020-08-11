
module Dolla.Consensus.Proposer.Receptionist.Service.OverEventStore
  ( persistClientRequest
  , persistClientRequests) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Dummy.Client.Request

import qualified Dolla.Consensus.Proposer.Receptionist.Service.Generic as Service.Generic
import           Dolla.Consensus.Log.EventStoreLog

persistClientRequest
  :: EventStoreClient.Dependencies
  -> ClientRequest
  -> IO ()
persistClientRequest eventStoreClientDependencies =
  Service.Generic.persistClientRequest
    (getEventStoreLog eventStoreClientDependencies LocalRequestLog)

persistClientRequests
  :: EventStoreClient.Dependencies
  -> NonEmpty ClientRequest
  -> IO ()
persistClientRequests eventStoreClientDependencies =
  Service.Generic.persistClientRequests
    (getEventStoreLog eventStoreClientDependencies LocalRequestLog)