{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Pipeline
  (detectingStarvation) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Log.LogNameIndex

import           Dolla.Consensus.Proposing.Detecting.Starvation.Execution.EventStore.Dependencies

import qualified Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.Pipeline as Generics

detectingStarvation
  :: ( S.MonadAsync m
     , MonadReader Dependencies m
     , MonadCatch m)
  => m ()
detectingStarvation = 
  ask 
  >>= \Dependencies {eventStoreClient} -> 
      Generics.detectingStarvation
        (getEventStoreLog eventStoreClient ProposingStarvingDetectionInputLog)
        (getEventStoreLog eventStoreClient ProposingStarvingDetectionOutputLog)


