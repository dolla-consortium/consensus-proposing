{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.StreamProcessing
  (detectStarvation ) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Input


import qualified Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.StateMachine as State

detectStarvation
  :: S.MonadAsync m
  => S.SerialT m Input
  -> S.SerialT m ()
detectStarvation
  = emitWhenPredicateValid
      State.projection
      State.starvingPredicate

emitWhenPredicateValid
  :: S.MonadAsync m
  => SF.Fold m Input state
  -> (state -> Bool)
  -> S.SerialT m Input
  -> S.SerialT m ()
emitWhenPredicateValid
  projection
  starvingInvariantPredicate
  inputStream
  = inputStream
    & S.postscan projection
    & S.filter starvingInvariantPredicate
    & S.map (const ())