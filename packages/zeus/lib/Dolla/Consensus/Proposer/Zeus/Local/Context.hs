module Dolla.Consensus.Proposer.Zeus.Local.Context
  (Context (..)) where

import           Dolla.Consensus.Common.Zeus.Local.Node

data Context
  = Context
    { rootFolder ::FilePath
    , node :: Node}