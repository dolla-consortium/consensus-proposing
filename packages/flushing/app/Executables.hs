{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( flushing
  ) where


import qualified Dolla.Consensus.Proposing.Flushing.FlushingOverEventStore as Notifying

flushing :: IO ()
flushing = Notifying.execute

