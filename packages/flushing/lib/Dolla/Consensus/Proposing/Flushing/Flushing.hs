{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Flushing.Flushing (flushing) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Common.Range
import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposing.Flushing.Dependencies
import qualified Dolla.Consensus.Proposing.Packaging.Input as Proposing.Packaging
import           Dolla.Consensus.Log.Aggregation
import           Data.Coerce (coerce)
import           Dolla.Common.NodeId
import           Dolla.Common.Offset
import           Dolla.Common.UUID.Deterministic
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Dolla.Consensus.Proposing.Flushing.Input

flushing
  :: ( MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , MonadIO m
     , S.MonadAsync m
     , MonadCatch m
     , Appendable a
     , UUIDProvider a )
  => log Input
  -> log (Proposing.Packaging.Input a)
  ->  S.SerialT m ()
flushing inputLog outputLog
  = stream infinitely inputLog
      & S.scan foldToDelta
      & S.filter (== 0)
      & S.indexed
      & S.mapM (\flushIndex -> do
          let itemId = getDeterministicUUID flushIndex
          void $ nonIdempotentAppend outputLog Proposing.Packaging.Flush {..})


foldToDelta :: MonadReader Dependencies m => SF.Fold m Input Offset
foldToDelta = (-) <$> foldToLastOffsetProduced <*> foldToLastLocalOffsetConsumed

foldToLastLocalOffsetConsumed :: MonadReader Dependencies m => SF.Fold m Input Offset
foldToLastLocalOffsetConsumed
  = Offset
    <$> SF.lmapM
          (\inputItem -> do
           Dependencies {nodeId}<- ask
           case inputItem of
             ProposalAccepted {byProposer = ByProposer {proposerId}} | proposerId == coerce nodeId -> return 1
             _ -> return 0)
           SF.sum

foldToLastOffsetProduced :: Monad m =>  SF.Fold m Input Offset
foldToLastOffsetProduced
  = Offset
    <$> SF.lmapM
          (\case
             LocalProposalProduced -> return 1
             _ -> return 0)
           SF.sum