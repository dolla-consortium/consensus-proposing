{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns , OverloadedStrings #-}

module Dolla.Consensus.EventStore.Zeus.Local.Settings
  ( getLocalSettings
  , EventStoreSettings (..)
  , mapToEventStoreSettings) where


import           Data.Text

import           Dolla.Common.Network.Core
import           Dolla.Common.Logging.Core

import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStoreClient

import           Dolla.Consensus.Common.Zeus.Local.Node
import           Dolla.Consensus.Common.Zeus.Local.Network

data EventStoreSettings
  = EventStoreSettings
    { nodeId :: NodeId
    , eventStoreUrl :: URL
    , projectionUrl :: URL
    , username :: Text
    , password :: Text}

getLocalSettings
  :: Node
  -> EventStoreSettings
getLocalSettings Node {..}
  = EventStoreSettings
    { nodeId
    , eventStoreUrl = URL { host = "127.0.0.1",port = getNodePort 1114 nodeIndex,path = ""}
    , projectionUrl = URL { host = "127.0.0.1",port = getNodePort 2113 nodeIndex,path = ""}
    , username = "admin"
    , password = "changeit"}

mapToEventStoreSettings :: EventStoreSettings -> LoggerSettings ->  EventStoreClient.Settings
mapToEventStoreSettings EventStoreSettings {..} logger = EventStoreClient.Settings { ..}

