{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  (receptioning) where


import qualified Dolla.Consensus.Proposing.Receptioning.Execution.EvenStore.Dolla.Warp.Server.Server as Receptioning


receptioning :: IO ()
receptioning = Receptioning.execute