{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Receptioning.API.Client.Settings (Settings (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Network.Core

import           Dolla.Common.NodeId

import           Dolla.Common.Logging.Core

data Settings 
  = Settings 
    { logger :: LoggerSettings 
    , nodeId :: NodeId
    , url :: URL}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings
  
