{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Pipe
  (detectTensedFlow ) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Input
import           Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Output

import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.StateMachine as State

detectTensedFlow
  :: S.MonadAsync m
  => S.SerialT m Input
  -> S.SerialT m Output
detectTensedFlow
  = emitWhenPredicateValid
      State.projection
      State.isFlowTensed

emitWhenPredicateValid
  :: S.MonadAsync m
  => SF.Fold m Input state
  -> (state -> Bool)
  -> S.SerialT m Input
  -> S.SerialT m Output
emitWhenPredicateValid
  projection
  predicate
  inputStream
  = inputStream
    & S.postscan projection
    & S.filter predicate
    & S.map (const Tensed)