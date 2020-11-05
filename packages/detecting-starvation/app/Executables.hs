{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingStarvation
  ) where


import qualified Dolla.Consensus.Proposing.Starving.Detecting.Execution.EventStore.Execute as StarvingDetection

detectingStarvation :: IO ()
detectingStarvation = StarvingDetection.execute

