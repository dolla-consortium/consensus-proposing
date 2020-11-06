
module Dolla.Consensus.Proposing.Receptioning.Execution.EvenStore.Service
  (receptioning) where

import           Data.List.NonEmpty
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Request

import qualified Dolla.Consensus.Proposing.Receptioning.Service.Service as Generic
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Control.Monad.IO.Class
import           Dolla.Consensus.Log.LogNameIndex

receptioning
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
receptioning eventStoreClient =
  Generic.receptioning
    (getEventStoreLog eventStoreClient ProposingReceptioningOutputLog)