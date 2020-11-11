{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.DetectingTension.Pipeline.Pipeline
  (measuringTension) where

import           Prelude hiding (log,writeFile)

import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine


import           Dolla.Consensus.Proposing.DetectingTension.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.DetectingTension.Pipeline.IO.Output
import           Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Pipe (detectTensedFlow)
import           Dolla.Consensus.Proposing.DetectingTension.Pipeline.Sinking.Pipe (sinking)
import           Dolla.Consensus.Proposing.DetectingTension.Pipeline.Welding.BluePrint ()
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
