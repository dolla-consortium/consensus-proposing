{-# LANGUAGE DuplicateRecordFields #-}
module Executables (simulating) where

import qualified Dolla.Consensus.Proposing.Simulating.Simulating as Simulating

simulating :: IO ()
simulating = Simulating.execute

