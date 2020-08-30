{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Assign
  ( Assignment (..)
  , AssigningState (..)
  , sameAssignment
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

type RequestSerialized = [Word8]

data AssigningState
  = AssigningState
    { currentLocalOffset :: Offset
    , currentRequestSerialized :: RequestSerialized
    , currentProposalSize :: Byte}

data Assignment
  = Assignment
    { localProposalOffset :: Offset
    , requestByteChunk :: Word8} deriving Show

assign
  :: ( MonadReader Dependencies m
     , Monad m)
  => S.SerialT m RequestSerialized
  -> S.SerialT m Assignment
assign input
  = input
    & S.postscan
        (Fold
          (\AssigningState {currentLocalOffset,currentProposalSize} currentRequestSerialized -> do
              let requestSize  = fromIntegral $ length currentRequestSerialized
              Dependencies {proposalSizeLimit} <- ask
              if (currentProposalSize + requestSize) > proposalSizeLimit
              then return AssigningState
                          { currentProposalSize = 0
                          , currentLocalOffset = nextOffset currentLocalOffset
                          , currentRequestSerialized }
              else return AssigningState
                          { currentProposalSize = currentProposalSize + requestSize
                          , currentLocalOffset
                          , currentRequestSerialized})
          (return AssigningState
            { currentLocalOffset = 0
            , currentProposalSize = 0
            , currentRequestSerialized = []})
          (\AssigningState {..}
              -> return $ (\requestByteChunk -> Assignment {localProposalOffset = currentLocalOffset, requestByteChunk} ) <$> currentRequestSerialized))
    & S.concatMap S.fromList

sameAssignment
  :: Assignment
  -> Assignment
  -> Bool
sameAssignment
  Assignment {localProposalOffset = previous}
  Assignment {localProposalOffset = current}
  = previous == current

