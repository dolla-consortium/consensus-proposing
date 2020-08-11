{-# LANGUAGE DuplicateRecordFields #-}
module Executables (clientSimulator) where

import qualified Dolla.Consensus.Proposer.Client.Simulator.Simulator as CLI.Simulator

clientSimulator :: IO ()
clientSimulator = CLI.Simulator.execute

