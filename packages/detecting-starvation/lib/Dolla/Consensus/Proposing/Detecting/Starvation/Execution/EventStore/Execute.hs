{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Execute (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposing.Detecting.Starvation.Settings
import           Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Dependencies
import           Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Junction (loadJunctionInEventStore)

import           Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Pipeline (detectingStarvation)

execute :: IO ()
execute = executeMicroservice 
            (\Settings {logger} -> logger) 
            executePipeline
  where 
      executePipeline :: ReaderT Dependencies IO ()
      executePipeline 
        = do
          Dependencies {eventStoreClient,nodeId,logger} <- ask
          withReaderT (const (nodeId, eventStoreClient)) loadJunctionInEventStore
          log logger INFO "Starting Service"
          detectingStarvation
          log logger INFO "Service Down"

