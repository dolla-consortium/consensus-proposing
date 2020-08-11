{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  (receptionist) where


import qualified Dolla.Consensus.Proposer.Receptionist.API.Server.Server as Receptionist


receptionist :: IO ()
receptionist = Receptionist.execute