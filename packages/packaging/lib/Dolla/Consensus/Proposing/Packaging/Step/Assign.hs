{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Assign
  ( Assignment (..)
  , AssigningState (..)
  , sameLocalProposalSpace
  , assign)
  where

import           Prelude hiding (log,writeFile)

import           Control.Monad.RWS.Class
import           Data.Function ((&))
import           Data.Word (Word8)

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Streamly.Internal.Data.Fold.Types

import           Dolla.Common.Offset
import           Dolla.Common.Memory.Byte (Byte)

import           Dolla.Consensus.Proposing.Packaging.Dependencies
import           Dolla.Consensus.Proposing.Packaging.Input

type RequestSerialized = [Word8]

data AssigningState
  = AssigningState
    { commit :: Bool
    , localProposalIdInProgress :: Offset
    , localProposalInProgressSize :: Byte
    , requestSerialized :: RequestSerialized}

data Assignment
  = Stage
    { localProposalOffset :: Offset
    , requestByteChunk :: Word8}
  | Commit  deriving Show

assign
  :: ( MonadReader Dependencies m
     , Monad m)
  => S.SerialT m (Input RequestSerialized)
  -> S.SerialT m Assignment
assign input = input & S.postscan assigning & S.concatMap S.fromList

assigning
  :: ( MonadReader Dependencies m
     , Monad m)
  => Fold m (Input RequestSerialized) [Assignment]
assigning =
  Fold
    (\AssigningState {localProposalIdInProgress,localProposalInProgressSize} input ->
        case input of
        LocalProposalConsumed {offsetConsumed} ->
          if nextOffset offsetConsumed == localProposalIdInProgress
          then return AssigningState { commit = True, requestSerialized = [], .. }
          else return AssigningState { commit = False, requestSerialized = [], .. }
        RequestData requestSerialized ->  do
          let requestSize  = fromIntegral $ length requestSerialized
          Dependencies {proposalSizeLimit} <- ask
          if (localProposalInProgressSize + requestSize) > proposalSizeLimit
          then return AssigningState
                      { localProposalInProgressSize = 0
                      , localProposalIdInProgress = nextOffset localProposalIdInProgress
                      , requestSerialized
                      , commit = False}
          else return AssigningState
                      { localProposalInProgressSize = localProposalInProgressSize + requestSize
                      , localProposalIdInProgress
                      , requestSerialized
                      , commit = False})
    (return AssigningState
      { localProposalIdInProgress = 0
      , localProposalInProgressSize = 0
      , requestSerialized = []
      , commit = False})
    (\AssigningState {commit,..} ->
        if commit
        then return [Commit]
        else return $ (\requestByteChunk -> Stage {localProposalOffset = localProposalIdInProgress, requestByteChunk} ) <$> requestSerialized)

sameLocalProposalSpace
  :: Assignment
  -> Assignment
  -> Bool
sameLocalProposalSpace
  Stage {localProposalOffset = previous}
  Stage {localProposalOffset = current} = previous == current

sameLocalProposalSpace _ Commit = True
sameLocalProposalSpace Commit _ = False
