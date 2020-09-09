{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Proposing.Simulating.Simulating
  (execute)
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           Control.Monad.Reader



import           Data.UUID ()

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposing.Simulating.Dependencies
import           Dolla.Consensus.Proposing.Simulating.Settings
import           Dolla.Consensus.Proposing.Simulating.StressLoad
import           Dolla.Consensus.Proposing.Simulating.OverFlowing
import           Dolla.Consensus.Proposing.Simulating.UnderSupplying
execute :: IO ()
execute
  = executeMicroservice
      (\Settings {logger} -> logger)
      start

start :: ReaderT Dependencies IO()
start
  = do
  Dependencies { logger, stressLoad } <- ask
  log logger INFO "Starting Simulating"
  case stressLoad of
    OverFlowing proposalSizeLimit -> overflowing proposalSizeLimit
    UnderSupplying proposalSizeLimit -> underSupplying proposalSizeLimit
    AlternatingStress _ -> return ()
  log logger INFO "End Simulating"

