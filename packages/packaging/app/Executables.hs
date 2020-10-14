{-# LANGUAGE DuplicateRecordFields #-}
module Executables
  ( packaging
  ) where

import qualified Dolla.Consensus.Proposing.Packaging.OverDolla  as Packaging

packaging :: IO ()
packaging = Packaging.execute

