{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposing.Client.Simulator.Settings (Settings(..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Logging.Core

import qualified Dolla.Consensus.Proposing.Receptioning.API.Client.Settings as Receptionist.Client

data Settings
  = Settings
    { logger :: LoggerSettings
    , receptioningClient :: Receptionist.Client.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

