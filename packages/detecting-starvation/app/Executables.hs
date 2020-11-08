{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingStarvation
  ) where


import qualified Dolla.Consensus.Proposing.MeasuringTension.Instances.EventStore.Execute as StarvingDetection

detectingStarvation :: IO ()
detectingStarvation = StarvingDetection.execute

