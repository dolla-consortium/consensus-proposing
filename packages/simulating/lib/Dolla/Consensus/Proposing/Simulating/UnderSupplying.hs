{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Simulating.UnderSupplying
  ( underSupplying
  , Context (..))
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import           Data.UUID ()
import           Data.Function ((&))
import           Data.Coerce (coerce)
import           Data.List.NonEmpty (NonEmpty)

import qualified Streamly.Prelude as S
import qualified Streamly as S

import           Dolla.Common.Logging.Core
import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Common.NodeId
import           Dolla.Common.Offset
import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine

import           Dolla.Consensus.Log.Aggregation
import           Dolla.Consensus.Dummy.Client.Request
import qualified Dolla.Consensus.Maestro.Output as Maestro
import qualified Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Client.Client
import qualified Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output as Packaging
import           Dolla.Consensus.Proposing.Simulating.GenRequest

data Context log
  = Context
    { logger :: Logger
    , receptioningClient :: Receptionist.Client.Dependencies
    , packagingOutputLog :: log Packaging.Output
    , getMaestroOutputLog :: ByBlockOffset -> log Maestro.Output}

underSupplying
  :: ( MemoryStreamLoggable m log
     , MonadReader (Context log) m
     , S.MonadAsync m
     , MonadCatch m)
    => Byte
    -> S.SerialT m ()
underSupplying proposalSizeLimit 
  = do
    Context {packagingOutputLog,logger} <- ask
    lift $ do 
      generateRequests (proposalSizeLimit `div` 2) >>= sendSimulatedRequest
      log logger INFO "Requests sent."
      _ <- simulateFirstConsensusReached
      log logger INFO "First Consensus Reached Simulated."
    stream infinitely packagingOutputLog
        & S.mapM (\Packaging.LocalProposalStaged {localOffset} -> do
          log logger INFO $ "Local Proposal " ++ show localOffset ++ " Staged."
          generateRequests (proposalSizeLimit `div` 2) >>= sendSimulatedRequest
          log logger INFO "Requests sent."
          simulateLocalProposalConsumptionForBlock (localOffset + 2)
          log logger INFO "Local Proposal Consumption Simulated."
          simulateConsensusReachedForBlock         (localOffset + 2)
          log logger INFO "Consensus Reached Simulated.")

simulateFirstConsensusReached
  :: ( MemoryStreamLoggable m log
     , MonadReader (Context log) m)
    => m ()
simulateFirstConsensusReached 
  = do
    Context {getMaestroOutputLog} <- ask
    let byBlockOffset = ByBlockOffset 1
        maestroLog = getMaestroOutputLog byBlockOffset
    void $ append maestroLog 0 (Maestro.ConsensusReached byBlockOffset)

simulateLocalProposalConsumptionForBlock
  :: ( MemoryStreamLoggable m log
     , MonadReader (Context log) m)
    => Offset
    -> m ()
simulateLocalProposalConsumptionForBlock blockOffset
  = do
    Context{getMaestroOutputLog, receptioningClient = Receptionist.Client.Dependencies {nodeId}} <- ask 
    let byBlockOffset = ByBlockOffset {blockOffset}
        byProposer  = ByProposer { blockOffset = blockOffset  , proposerId = coerce nodeId }
        maestroLog = getMaestroOutputLog byBlockOffset
    void $ append maestroLog 0 (Maestro.ProposalAccepted {byProposer})
    
simulateConsensusReachedForBlock
  :: ( MemoryStreamLoggable m log
     , MonadReader (Context log) m)
    => Offset
    -> m ()
simulateConsensusReachedForBlock blockOffset
  = do
    Context{getMaestroOutputLog} <- ask 
    let byBlockOffset = ByBlockOffset {blockOffset}
        maestroLog = getMaestroOutputLog byBlockOffset
    void $ append maestroLog 1 (Maestro.ConsensusReached byBlockOffset)

sendSimulatedRequest
  :: ( MonadIO m
     , MonadReader (Context log) m)
  => NonEmpty DollaClientRequest
  -> m ()
sendSimulatedRequest requests
  = do
    Context { logger,receptioningClient = Receptionist.Client.Dependencies {url, httpClientManager}} <- ask
    log logger INFO $ "Sending " ++ show (length requests) ++ " requests."
    void $ sendProposalRequests httpClientManager url requests
    