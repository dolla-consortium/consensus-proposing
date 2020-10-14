{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Proposing.Simulating.OverFlowing
  (overflowing)
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S

import           Data.UUID ()
import           Data.Function ((&))

import           Dolla.Common.Logging.Core

import qualified Dolla.Consensus.Proposing.Receptioning.API.Client.Dependencies as Receptionist.Client
import           Dolla.Consensus.Proposing.Receptioning.API.Client.Client
import           Dolla.Consensus.Proposing.Simulating.Dependencies
import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Consensus.Proposing.Simulating.GenRequest

overflowing :: MonadIO m => Byte -> ReaderT Dependencies m ()
overflowing proposalSizeLimit
  = do
    Dependencies { logger ,receptioningClient = Receptionist.Client.Dependencies {url, httpClientManager}} <- ask
    log logger INFO "Simulating an overflowing of request"
    liftIO $ S.drain
      $ S.fromList [(1 :: Integer) ..]
      & S.mapM
         (\actionIndex -> do
            requests <- generateRequests $ 2 * proposalSizeLimit
            let nbRequest  :: Integer = fromIntegral $ length requests
            _ <- sendProposalRequests httpClientManager url requests
            log logger INFO $
              "[Sent " ++ show nbRequest ++ " requests " ++
              " | total : " ++ show (actionIndex * toInteger nbRequest) ++ " ]")