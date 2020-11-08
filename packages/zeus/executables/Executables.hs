{-# LANGUAGE DuplicateRecordFields #-}
module Executables (executeZeus) where

import           Dolla.Consensus.Proposing.Zeus.CLI

executeZeus :: IO ()
executeZeus = startInteractiveZeus

