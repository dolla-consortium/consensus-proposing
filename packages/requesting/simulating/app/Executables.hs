{-# LANGUAGE DuplicateRecordFields #-}
module Executables (requestingSimulating) where

import qualified Dolla.Consensus.Proposing.Requesting.Simulating.Simulating as Requesting.Simulating

requestingSimulating :: IO ()
requestingSimulating = Requesting.Simulating.execute

