{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( packaging
  ) where


import qualified Dolla.Consensus.Proposing.Packaging.PackagingOverEventStore as Packaging

packaging :: IO ()
packaging = Packaging.execute

