{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposing.Simulating.Settings (Settings(..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Logging.Core
import           Dolla.Consensus.Proposing.Simulating.StressLoad
import qualified Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Client.Settings as Receptionist.Client
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore

data Settings
  = Settings
    { logger :: LoggerSettings
    , receptioningClient :: Receptionist.Client.Settings
    , stressLoad :: StressLoad
    , eventStoreClient :: EventStore.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

