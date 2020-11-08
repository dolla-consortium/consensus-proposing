{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingTension
  ) where


import qualified Dolla.Consensus.Proposing.DetectingTension.Instances.EventStore.Execute as DetectingTension

detectingTension :: IO ()
detectingTension = DetectingTension.execute

