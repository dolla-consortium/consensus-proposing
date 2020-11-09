{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Executables
  ( execute
  ) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposing.DetectingTension.Execution.Environment.EventStore.Settings
import           Dolla.Consensus.Proposing.DetectingTension.Execution.Environment.EventStore.Dependencies
import           Dolla.Consensus.Proposing.DetectingTension.Execution.Environment.EventStore.Junction (loadJunctionInEventStore)

import           Dolla.Consensus.Proposing.DetectingTension.Execution.Environment.EventStore.Pipeline (measuringTension)
import qualified Streamly.Prelude as S

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
          S.drain measuringTension
          log logger INFO "Service Down"
