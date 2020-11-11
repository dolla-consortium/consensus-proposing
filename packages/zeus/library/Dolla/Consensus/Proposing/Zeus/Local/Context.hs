module Dolla.Consensus.Proposing.Zeus.Local.Context
  (Context (..)) where

import           Dolla.Consensus.Common.Zeus.Local.Node
import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Consensus.Proposing.Simulating.StressLoad

data Context
  = Context
    { rootFolder ::FilePath
    , proposalSizeLimit :: Byte
    , stressLoad :: StressLoad
    , node :: Node}