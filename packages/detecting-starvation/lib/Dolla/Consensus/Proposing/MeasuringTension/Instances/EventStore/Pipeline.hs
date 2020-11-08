{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.MeasuringTension.Instances.EventStore.Pipeline
  (measuringTension) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Log.LogNameIndex

import           Dolla.Consensus.Proposing.MeasuringTension.Instances.EventStore.Dependencies

import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Pipeline as Generics

measuringTension
  :: ( S.MonadAsync m
     , MonadReader Dependencies m
     , MonadCatch m)
  => S.SerialT m ()
measuringTension = 
  ask 
  >>= \Dependencies {eventStoreClient} -> 
      Generics.measuringTension
        (getEventStoreLog eventStoreClient ProposingStarvingDetectionInputLog)
        (getEventStoreLog eventStoreClient ProposingStarvingDetectionOutputLog)


