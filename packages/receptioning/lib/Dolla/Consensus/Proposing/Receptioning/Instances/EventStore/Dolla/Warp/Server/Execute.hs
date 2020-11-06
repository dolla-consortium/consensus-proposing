{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Server.Execute (execute) where

import           Prelude hiding (log)

import           Dolla.Common.Executable.Executable

import qualified Dolla.Consensus.Proposing.Receptioning.API.Server.Settings as Server

import           Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Server.Server (runServerOnWarp)

execute :: IO ()
execute = executeMicroservice 
            (\Server.Settings {logger} -> logger) 
            runServerOnWarp
