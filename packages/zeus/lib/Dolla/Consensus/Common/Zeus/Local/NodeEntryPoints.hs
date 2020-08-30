{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Common.Zeus.Local.NodeEntryPoints
  ( EntryPointsSettings (..)
  , getLocalConfiguration
  , getLocalConfigurations) where

import           Data.Function ((&))

import           Dolla.Common.NodeId
import           Dolla.Common.Network.Core
import           Dolla.Consensus.Common.Zeus.Local.Node 
import           Dolla.Consensus.Common.Zeus.Local.Network

data EntryPointsSettings
  = EntryPointsSettings
    { nodeId :: NodeId
    , statusServerUrl :: URL
    , downloadServerUrl :: URL
    , votingBroadcastUrl :: URL
    , receptioningUrl :: URL}

getLocalConfiguration
  :: Node
  -> EntryPointsSettings
getLocalConfiguration Node {..}
  = EntryPointsSettings
    { nodeId
    , statusServerUrl = URL { host = "127.0.0.1",port = getNodePort 10000 nodeIndex,path = ""}
    , downloadServerUrl = URL { host = "127.0.0.1",port = getNodePort 10500 nodeIndex,path = ""}
    , votingBroadcastUrl = URL { host = "127.0.0.1",port = getNodePort 11000 nodeIndex,path = ""}
    , receptioningUrl = URL { host = "127.0.0.1",port = getNodePort 11500 nodeIndex,path = ""}}

getLocalConfigurations
  :: Int
  -> [EntryPointsSettings]
getLocalConfigurations numberOfNodes
  = [0 .. numberOfNodes - 1] & map (getLocalConfiguration . getNode)


