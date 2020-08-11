{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Proposer.Client.Simulator.Simulator
  (execute)
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           System.Random
import           Data.List.NonEmpty (NonEmpty (..),fromList)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S

import           Data.UUID ()
import           Data.Function ((&))

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Dummy.Client.Request
import qualified Dolla.Consensus.Proposer.Receptionist.API.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposer.Receptionist.API.Client.Client
import           Dolla.Consensus.Proposer.Client.Simulator.Dependencies
import           Dolla.Consensus.Proposer.Receptionist.API.Client.Dependencies (nodeId)
import           Dolla.Consensus.Proposer.Client.Simulator.Settings

execute :: IO ()
execute
  = executeMicroservice
      (\Settings {logger} -> logger)
      start

start :: ReaderT Dependencies IO()
start
  = do
  Dependencies {logger, receptionistClient = Receptionist.Client.Dependencies {nodeId,url, httpClientManager}} <- ask
  log logger INFO $ "Starting Client Simulator to " ++ show nodeId
  lift $
    S.drain $
    S.fromList [(1 :: Integer) ..] &
    S.mapM
     (\actionIndex -> do
        commandId <- randomIO
        source <- randomIO
        destination <- randomIO
        let nbRequestSent :: Int = 10000
        let requests :: NonEmpty ClientRequest = fromList $ replicate nbRequestSent SpendMoney {amount = 100, ..}
        _ <- sendProposalRequests httpClientManager url requests
        log logger INFO $
          "[Sent " ++
          show nbRequestSent ++
          " requests " ++
          " | total : " ++ show (actionIndex * toInteger nbRequestSent) ++ " ]")

  log logger INFO "End Client Simulator"
