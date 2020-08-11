{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposer.Client.Simulator.Settings (Settings(..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Logging.Core

import qualified Dolla.Consensus.Proposer.Receptionist.API.Client.Settings as Receptionist.Client

data Settings
  = Settings
    { logger :: LoggerSettings
    , receptionistClient :: Receptionist.Client.Settings}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Settings

