{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( detectingStarvation
  ) where


import qualified Dolla.Consensus.Proposing.Starving.Detecting.OverEventStore as Notifying

detectingStarvation :: IO ()
detectingStarvation = Notifying.execute

