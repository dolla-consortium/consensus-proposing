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

import           Data.Function ((&))
import           Data.Word (Word8)

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Dolla.Common.Offset

import           Dolla.Common.Memory.Byte (Byte)

import           Streamly.Internal.Data.Fold.Types

data AssigningState
  = AssigningState
    { currentLocalOffset :: Offset
    , currentRequest :: [Word8]
    , currentProposalSize :: Byte}

data Assignment
  = Assignment
    { localProposalOffset :: Offset
    , content :: Word8} deriving Show

assign
  :: Monad m
  => S.SerialT m [Word8]
  -> S.SerialT m Assignment
assign input
  = input
    & S.postscan
        (Fold
          (\AssigningState {..} requestInBytes -> do
              let proposalSizeLimit = 1000 * 1024 :: Byte -- 1 MB
                  requestSize  = fromIntegral $ length requestInBytes
              if (currentProposalSize + requestSize) > proposalSizeLimit
              then return AssigningState
                          { currentProposalSize = 0
                          , currentLocalOffset = nextOffset currentLocalOffset
                          , currentRequest = requestInBytes}
              else return AssigningState
                          { currentProposalSize = currentProposalSize + requestSize
                          , currentLocalOffset
                          , currentRequest = requestInBytes})
          (return AssigningState
            { currentLocalOffset = 0
            , currentProposalSize = 0
            , currentRequest = []})
          (\AssigningState {..}
              -> return $ (\requestChunk -> Assignment {localProposalOffset = currentLocalOffset, content = requestChunk} ) <$> currentRequest))
    & S.concatMap S.fromList

sameAssignment
  :: Assignment
  -> Assignment
  -> Bool
sameAssignment
  Assignment {localProposalOffset = previous}
  Assignment {localProposalOffset = current}
  = previous == current

