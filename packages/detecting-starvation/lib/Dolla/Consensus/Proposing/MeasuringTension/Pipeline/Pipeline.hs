{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Pipeline
  (measuringTension) where

import           Prelude hiding (log,writeFile)

import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine


import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.IO.Output
import           Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Pipe (detectTensedFlow)
import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Sinking.Pipe (sinking)
import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Welding.BluePrint ()
import           Dolla.Common.Pipeline.Weldable ((~>),(.~>))

measuringTension
  :: ( MemoryStreamLoggable m log
     , S.MonadAsync m
     , MonadCatch m)
  => log Input 
  -> log Output
  -> S.SerialT m ()
measuringTension inputLog outputLog 
  = stream infinitely inputLog -- sourcing
      ~> detectTensedFlow
     .~> sinking outputLog
