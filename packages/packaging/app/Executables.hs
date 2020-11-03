{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( packaging
  ) where

import qualified Dolla.Consensus.Proposing.Packaging.Microservice  as Packaging

packaging :: IO ()
packaging = Packaging.execute

