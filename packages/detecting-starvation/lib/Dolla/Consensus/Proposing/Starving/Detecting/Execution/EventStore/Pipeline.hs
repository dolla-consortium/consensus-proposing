{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Starving.Detecting.Execution.EventStore.Pipeline
  (detectingStarvation) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog (EventStoreLog)

import           Dolla.Consensus.Proposing.Starving.Detecting.Execution.EventStore.Dependencies

import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Output


import qualified Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.Pipeline as Generics

detectingStarvation
  :: ( S.MonadAsync m
     , MonadReader Dependencies m
     , MonadCatch m)
  => m ()
detectingStarvation = do
  Dependencies {eventStoreClient} <- ask
  let inputLog  = getEventStoreLog eventStoreClient ProposingStarvingDetectionInputLog
      outputLog = getEventStoreLog eventStoreClient LocalRequestLog :: EventStoreLog Output
  Generics.detectingStarvation
    inputLog
    outputLog


