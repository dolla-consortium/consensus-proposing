{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  (receptioning) where


import qualified Dolla.Consensus.Proposing.Receptioning.API.Server.Server as Receptioning


receptioning :: IO ()
receptioning = Receptioning.execute