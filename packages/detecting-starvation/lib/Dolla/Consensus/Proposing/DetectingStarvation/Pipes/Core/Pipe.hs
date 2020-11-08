{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.Pipe
  (detectStarvation ) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input


import qualified Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.StateMachine as State

detectStarvation
  :: S.MonadAsync m
  => S.SerialT m Input
  -> S.SerialT m ()
detectStarvation
  = emitWhenPredicateValid
      State.projection
      State.isNewLocalProposalAsked

emitWhenPredicateValid
  :: S.MonadAsync m
  => SF.Fold m Input state
  -> (state -> Bool)
  -> S.SerialT m Input
  -> S.SerialT m ()
emitWhenPredicateValid
  projection
  predicate
  inputStream
  = inputStream
    & S.postscan projection
    & S.filter predicate
    & S.map (const ())