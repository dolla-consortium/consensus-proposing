{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Dolla.Consensus.Proposing.Simulating.Simulating
  (execute)
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           Data.UUID ()
import           Control.Monad.Reader

import qualified Streamly.Internal.Prelude as S

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Log.LogNameIndex
import           Dolla.Consensus.Maestro.ESMerger (loadMaestroInputProjection)

import qualified Dolla.Consensus.Proposing.Receptioning.Execution.EvenStore.Dolla.Warp.Client.Dependencies as Receptionist.Client

import           Dolla.Consensus.Proposing.Simulating.Dependencies
import           Dolla.Consensus.Proposing.Simulating.Settings
import           Dolla.Consensus.Proposing.Simulating.StressLoad
import           Dolla.Consensus.Proposing.Simulating.OverFlowing
import           Dolla.Consensus.Proposing.Simulating.UnderSupplying as UnderSupplying


execute :: IO ()
execute
  = executeMicroservice
      (\Settings {logger} -> logger)
      start

start :: ReaderT Dependencies IO()
start
  = do
  Dependencies
    { logger
    , stressLoad
    , eventStoreClient
    , receptioningClient = receptioningClient@Receptionist.Client.Dependencies {nodeId} } <- ask
  log logger INFO "Starting Simulating"
  case stressLoad of
    OverFlowing proposalSizeLimit -> overflowing proposalSizeLimit
    UnderSupplying proposalSizeLimit -> do
      -- Need to run the merge of the maestro to simulate the merge of the detecting starvation merge
      _ <- withReaderT (const (nodeId, eventStoreClient)) loadMaestroInputProjection
      S.drain
        $ S.runReaderT
            UnderSupplying.Context 
              { packagingOutputLog = getEventStoreLog eventStoreClient ProposingPackagingOutputLog 
              , getMaestroOutputLog = getEventStoreLog eventStoreClient . MaestroOutputLog
              , ..} 
            (underSupplying proposalSizeLimit)
  log logger INFO "End Simulating"

