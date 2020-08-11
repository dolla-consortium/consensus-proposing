{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposer.Bucketizer.BucketizerOverEventStore (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Streamly.Prelude as S

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposer.Bucketizer.Bucketizer
import           Dolla.Consensus.Proposer.Bucketizer.Settings
import           Dolla.Consensus.Proposer.Bucketizer.Dependencies
import           Dolla.Consensus.Log.EventStoreLog
import qualified Streamly.Internal.Prelude as SIP


execute :: IO ()
execute = executeMicroservice (\Settings {logger} -> logger) start

-- | EntryPoint of the Proposer : 
--     * proposalProposer Stream processor plugged with streams from the Event store
start :: ReaderT Dependencies IO ()
start = do
  d@Dependencies {eventStoreClient,logger} <- ask
  log logger INFO "Proposal Maker up and running"
  lift $ drain
    $ SIP.runReaderT
        d
        (requestBucketizer
          (getEventStoreLog eventStoreClient LocalRequestLog)
          (getEventStoreLog eventStoreClient ProposerOutputLog))
  log logger INFO "Proposal Maker down"

