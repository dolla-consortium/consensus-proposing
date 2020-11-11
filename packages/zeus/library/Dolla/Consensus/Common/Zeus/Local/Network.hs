module Dolla.Consensus.Common.Zeus.Local.Network (getNodePort) where


import          Data.Coerce (coerce)
import          Dolla.Common.Network.Core
import          Dolla.Consensus.Common.Zeus.Local.NodeIndex

getNodePort
  :: URLPort
  -> NodeIndex
  -> URLPort
getNodePort p i = URLPort (coerce i + coerce p)