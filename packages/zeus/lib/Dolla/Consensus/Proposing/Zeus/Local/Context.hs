module Dolla.Consensus.Proposing.Zeus.Local.Context
  (Context (..)) where

import           Dolla.Consensus.Common.Zeus.Local.Node
import           Dolla.Common.Memory.Byte (Byte)

data Context
  = Context
    { rootFolder ::FilePath
    , proposalSizeLimit :: Byte
    , node :: Node}