{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Pipe
  (serializing)
  where

import           Prelude
import           Data.ByteString hiding (length,map,append)
import           Data.Coerce (coerce)
import           Data.Aeson.Types (ToJSON)

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Common.UUID.Deterministic
import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Input  as Input
import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Output as Output
import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.SerializedRequest

serializing
  :: ( Monad m, ToJSON a)
  => S.SerialT m (Input a)
  -> S.SerialT m (Output SerializedRequest)
serializing 
  =  S.map (\case 
      ForceProposalProduction -> ProposalProductionNotForced -- Isomorphism with Maybe
      Serialize request -> Serialized $ (coerce . unpack . getEncodedItem) request)