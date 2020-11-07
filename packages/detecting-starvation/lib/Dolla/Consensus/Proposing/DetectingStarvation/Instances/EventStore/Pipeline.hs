{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.DetectingStarvation.Instances.EventStore.Pipeline
  (detectingStarvation) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Log.LogNameIndex

import           Dolla.Consensus.Proposing.DetectingStarvation.Instances.EventStore.Dependencies

import qualified Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.Pipeline as Generics

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


