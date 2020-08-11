{-# LANGUAGE DuplicateRecordFields #-}
module Executables (executeZeus) where

import           Dolla.Consensus.Proposer.Zeus.CLI

executeZeus :: IO ()
executeZeus = startInteractiveZeus

