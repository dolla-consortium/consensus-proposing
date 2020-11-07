{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Dolla.Execute (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SIP

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Dolla.Pipeline (packaging)
import           Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Settings
import           Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Dependencies
import           Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Dolla.Junction
execute :: IO ()
execute
  = executeMicroservice
      (\Settings {logger} -> logger)
      executePipeline
  where 
    executePipeline :: ReaderT Dependencies IO ()
    executePipeline = do
      dependencies @ Dependencies {eventStoreClient,nodeId,logger} <- ask
      withReaderT (const (nodeId, eventStoreClient)) loadJunctionInEventStore
      log logger INFO "Pipeline Starting"
      lift $ drain $ SIP.runReaderT dependencies packaging
      log logger INFO "End Of Pipeline"

