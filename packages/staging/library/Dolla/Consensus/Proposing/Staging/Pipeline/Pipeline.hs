{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Staging.Pipeline.Pipeline
  (staging)
  where

import           Prelude 
import           Data.Aeson.Types (ToJSON,FromJSON)

import           Control.Monad.Catch (MonadCatch)


import qualified Streamly as S

import           Dolla.Libraries.LogEngine.LogEngine (MemoryStreamLoggable (..))
import           Dolla.Common.Range (infinitely)

import           Dolla.Consensus.Proposing.Staging.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.Staging.Pipeline.IO.Output

import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Pipe  (serializing)
import           Dolla.Consensus.Proposing.Staging.Pipes.NonEmptying.Pipe  (nonEmptying)
import           Dolla.Consensus.Proposing.Staging.Pipes.Capping.Pipe      (capping)
import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Consensus.Proposing.Staging.Pipes.Persisting.Pipe   (persisting)
import           Dolla.Consensus.Proposal.Persistence (ProposalRootFolder)
import           Dolla.Consensus.Proposing.Staging.Pipeline.Sinking.Pipe (sinking)

import           Dolla.Common.Pipeline.Weldable ((~>),(.~>))
import           Dolla.Consensus.Proposing.Staging.Pipeline.Welding.BluePrint ()

-- | Staging Pipeline expressed in its most polymorphic way :
--  - Log Engine Agnostic
--  - Request Agnostic
staging
  :: ( ToJSON request
     , FromJSON request
     , Show request
     , MonadCatch m
     , S.MonadAsync m
     , MemoryStreamLoggable m log
     )
  => ProposalRootFolder
  -> Byte
  -> log (Input request)
  -> log Output
  -> S.SerialT m ()
staging
  proposalRootFolder
  proposalSizeLimit
  inputLog
  outputLog
  = stream infinitely inputLog -- sourcing
     ~> serializing
    .~> nonEmptying
    .~> capping proposalSizeLimit
    .~> persisting proposalRootFolder
    .~> sinking outputLog





