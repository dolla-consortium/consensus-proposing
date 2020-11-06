{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( packaging
  ) where

import qualified Dolla.Consensus.Proposing.Packaging.Instances.EventStore.Dolla.Execute as Packaging

packaging :: IO ()
packaging = Packaging.execute

