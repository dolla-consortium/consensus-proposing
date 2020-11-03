{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.Starving.Detecting.OverMemoryStream
  (detectingPipelineStarving) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Common.Offset
import           Dolla.Common.UUID.Deterministic

import           Dolla.Consensus.Proposing.Starving.Detecting.Input
import           Dolla.Consensus.Proposing.Starving.Detecting.Output

detectingPipelineStarving
  :: S.MonadAsync m
  => SF.Fold m Input state
  -> (state -> Bool)
  -> S.SerialT m Input
  -> S.SerialT m Output
detectingPipelineStarving
  projection
  starvingInvariantPredicate
  inputStream
  = inputStream
    & S.postscan projection
    & S.filter starvingInvariantPredicate
    & S.indexed
    & S.map (fromIntegralToOffset . fst )
    & S.mapM
      (\starvingOffset -> do
         let itemId = getDeterministicUUID starvingOffset
         return LocalProposalStarvationDetected {..})

