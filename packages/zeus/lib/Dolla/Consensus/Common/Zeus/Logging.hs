{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Common.Zeus.Logging (nodeLoggerId) where

import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core

nodeLoggerId :: NodeId -> Priority -> String -> LoggerSettings
nodeLoggerId NodeId {unNodeId}priority logName
  = LoggerSettings
     { priority = priority
     , loggerId = LoggerId $ "[" ++ unNodeId ++"]-[" ++ logName ++ "]"}

