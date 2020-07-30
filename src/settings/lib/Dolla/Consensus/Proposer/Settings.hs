{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposer.Settings (Settings (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore


data Settings
  = Settings
    { nodeId :: NodeId
    , logger :: LoggerSettings
    , proposalRootFolder :: FilePath
    , eventStoreClient :: EventStore.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

