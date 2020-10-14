{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Packaging.Serializing.StreamProcessing
  (serializing)
  where

import           Prelude
import           Data.ByteString hiding (length,map,append)
import           Data.Coerce (coerce)
import           Data.Aeson.Types (ToJSON)

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Common.UUID.Deterministic
import           Dolla.Consensus.Proposing.Packaging.Serializing.Input
import           Dolla.Consensus.Proposing.Packaging.Serializing.Output
import           Dolla.Consensus.Proposing.Packaging.Serializing.SerializedRequest

serializing
  :: ( Monad m, ToJSON a)
  => S.SerialT m (Input a)
  -> S.SerialT m (Output SerializedRequest)
serializing 
  =  S.map (\case 
      TransmitPipelineStarvingDownStream -> PipelineStarving
      RequestToSerialize request -> RequestSerialized $ (coerce . unpack . getEncodedItem) request) 