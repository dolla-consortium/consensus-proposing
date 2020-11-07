{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingStarvation
  ) where


import qualified Dolla.Consensus.Proposing.DetectingStarvation.Instances.EventStore.Execute as StarvingDetection

detectingStarvation :: IO ()
detectingStarvation = StarvingDetection.execute

