{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( staging
  ) where

import qualified Dolla.Consensus.Proposing.Staging.Instances.EventStore.Dolla.Execute as Staging

staging :: IO ()
staging = Staging.execute

