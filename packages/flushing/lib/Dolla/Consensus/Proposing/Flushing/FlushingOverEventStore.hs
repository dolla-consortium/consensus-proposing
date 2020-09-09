{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Flushing.FlushingOverEventStore (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SIP


import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog (EventStoreLog)
import           Dolla.Consensus.Proposing.Flushing.Flushing
import           Dolla.Consensus.Proposing.Flushing.Settings
import           Dolla.Consensus.Proposing.Flushing.Dependencies
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Request (Request)
import           Dolla.Consensus.Proposing.Flushing.ESMerger (loadFlushingInputProjection)
import qualified Dolla.Consensus.Proposing.Packaging.Input as Proposing

execute :: IO ()
execute = executeMicroservice (\Settings {logger} -> logger) start

start :: ReaderT Dependencies IO ()
start = do
  dependencies@Dependencies {eventStoreClient,nodeId,logger} <- ask
  _ <- withReaderT (const (nodeId, eventStoreClient)) loadFlushingInputProjection
  log logger INFO "Starting Service"
  lift $ drain
    $ SIP.runReaderT
        dependencies
        (flushing
          (getEventStoreLog eventStoreClient ProposingFlushingInput)
          (getEventStoreLog eventStoreClient LocalRequestLog :: EventStoreLog (Proposing.Input Request)) )
  log logger INFO "Service Down"

