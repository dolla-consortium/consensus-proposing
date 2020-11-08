{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  (receptioning) where


import qualified Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Server.Execute as Receptioning


receptioning :: IO ()
receptioning = Receptioning.execute