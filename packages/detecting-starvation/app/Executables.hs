{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingStarvation
  ) where


import qualified Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Execute as StarvingDetection

detectingStarvation :: IO ()
detectingStarvation = StarvingDetection.execute

