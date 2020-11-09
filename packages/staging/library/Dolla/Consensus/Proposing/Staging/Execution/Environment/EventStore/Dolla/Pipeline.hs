{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Dolla.Pipeline (staging) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import           Data.Data

import qualified Streamly as S

import qualified Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Pipeline as OverEventStore
import           Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Dependencies

import           Dolla.Consensus.Request
import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Consensus.Consortium.Request

staging
  :: ( MonadReader Dependencies m
     , S.MonadAsync m
     , MonadCatch m)
  => S.SerialT m ()
staging
  = OverEventStore.staging (Proxy :: Proxy (Request DollaClientRequest ConsortiumRequest))


