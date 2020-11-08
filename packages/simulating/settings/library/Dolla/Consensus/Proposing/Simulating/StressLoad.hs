{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposing.Simulating.StressLoad (StressLoad(..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Dolla.Common.Memory.Byte (Byte)

data StressLoad
  = OverFlowing {proposalSizeLimit :: Byte}
  | UnderSupplying {proposalSizeLimit :: Byte}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON StressLoad

