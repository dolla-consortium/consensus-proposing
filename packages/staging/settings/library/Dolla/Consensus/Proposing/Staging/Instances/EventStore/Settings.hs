{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Staging.Instances.EventStore.Settings (Settings (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Common.NodeId
import           Dolla.Common.Logging.Core
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore


data Settings
  = Settings
    { nodeId :: NodeId
    , logger :: LoggerSettings
    , proposalRootFolder :: FilePath
    , proposalSizeLimit :: Byte
    , eventStoreClient :: EventStore.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

