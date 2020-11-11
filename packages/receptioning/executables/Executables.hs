{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Executables
  (executeServer) where

import           Prelude hiding (log)

import           Dolla.Common.Executable.Executable

import qualified Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Server.Settings as Server

import           Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Server.Server (runServerOnWarp)

executeServer :: IO ()
executeServer = executeMicroservice
            (\Server.Settings {logger} -> logger)
            runServerOnWarp
