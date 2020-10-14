{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Starving.Detecting.OverEventStore (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog (EventStoreLog)

import           Dolla.Consensus.Proposing.Starving.Detecting.Settings
import           Dolla.Consensus.Proposing.Starving.Detecting.Dependencies
import           Dolla.Consensus.Proposing.Starving.Detecting.ESMerger (loadInputProjection)

import           Dolla.Consensus.Proposing.Starving.Detecting.Output

import qualified Dolla.Consensus.Proposing.Starving.Detecting.State as State
import qualified Dolla.Consensus.Proposing.Starving.Detecting.OverPersistedLog as OverPersistedLog
import qualified Dolla.Consensus.Proposing.Starving.Detecting.OverMemoryStream as OverMemoryStream

execute :: IO ()
execute = executeMicroservice (\Settings {logger} -> logger) start

start :: ReaderT Dependencies IO ()
start = do
  Dependencies {eventStoreClient,nodeId,logger} <- ask
  _ <- withReaderT (const (nodeId, eventStoreClient)) loadInputProjection
  log logger INFO "Starting Service"
  let inputLog  = getEventStoreLog eventStoreClient ProposingStarvingDetectionInputLog
      outputLog = getEventStoreLog eventStoreClient LocalRequestLog :: EventStoreLog Output
  OverPersistedLog.detectingPipelineStarving
    inputLog
    outputLog
    (OverMemoryStream.detectingPipelineStarving
                  State.projection
                  State.starvingInvariantPredicate)
  log logger INFO "Service Down"

