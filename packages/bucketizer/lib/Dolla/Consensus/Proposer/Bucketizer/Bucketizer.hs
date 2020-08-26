{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposer.Bucketizer.Bucketizer (genesis) where

import           Prelude hiding (log,writeFile)
import           Data.Function ((&))

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Dolla.Common.Range
import           Dolla.Common.Offset

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposer.Bucketizer.Dependencies
import           Dolla.Consensus.Proposer.Bucketizer.Output
import           Dolla.Consensus.Request
import           Control.Monad.Reader
import           Dolla.Common.UUID.Deterministic
import           Data.ByteString hiding (length,map,append)
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified System.IO as FH
import           Data.Word (Word8)
import           Control.Monad.Catch (MonadCatch)
import           System.Directory
import qualified Dolla.Common.Streamly as S (groupsBy2,lmap2)
import           Data.Aeson.Types (ToJSON)
import           GHC.IO.IOMode

import           Streamly.Internal.Data.Fold.Types
import Control.Monad.State (MonadState, put, get)
import Dolla.Common.Memory.Byte (Byte)
import Dolla.Common.Logging.Core


genesis
  :: ( MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , MonadIO m
     , S.MonadAsync m
     , MonadCatch m)
  => log Request
  -> log Output
  ->  S.SerialT m ()
genesis requestLog outputLog =
  streamWithLogItem infinitely requestLog
  & serialize
  & assign
  & persist
  & notify

  where notify = S.mapM (\localOffsetPersisted -> void $ append outputLog localOffsetPersisted LocalProposalProduced)

serialize
  :: ( MonadReader Dependencies m
     , MonadIO m
     , S.MonadAsync m
     , ToJSON a)
  => S.SerialT m a
  -> S.SerialT m [Word8]
serialize input =  input & S.map (unpack . getEncodedItem)

data AssigningState
  = AssigningState
    { currentLocalOffset :: Offset
    , currentRequest :: [Word8]
    , currentProposalSize :: Byte}

data Assignment = Assignment {localProposalOffset :: Offset, content :: Word8} deriving Show

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

persist
  :: ( MonadReader Dependencies m
     , S.MonadAsync m
     , Monad m)
  => S.SerialT m Assignment
  -> S.SerialT m Offset
persist inputStream = do
    Dependencies {proposalRootFolder} <- ask
    inputStream
      & S.liftInner
      & S.groupsBy2
          assignedWithingSameLocalProposal
          getLocalProposalFileHandle
          (S.lmap2 content IFH.write2)
      & S.evalStateT State
                      { localProposalRootFolder = proposalRootFolder ++ "local/"
                      , handleMaybe = Nothing
                      , localBlockOffset = 0}
      & S.indexed
      & S.mapM (\(blockOffset,()) -> do
          liftIO $ renameFile
            (proposalRootFolder ++ "local/" ++ show blockOffset ++ ".tmp")
            (proposalRootFolder ++ "local/" ++ show blockOffset ++ ".proposal")
          return $ fromIntegralToOffset blockOffset)

assignedWithingSameLocalProposal
  :: Assignment
  -> Assignment
  -> Bool
assignedWithingSameLocalProposal
  Assignment {localProposalOffset = previous}
  Assignment {localProposalOffset = current}
  = previous == current

data State
  = State
    { localProposalRootFolder::FilePath
    , handleMaybe :: Maybe FH.Handle
    , localBlockOffset :: Offset }

getLocalProposalFileHandle
  :: ( MonadIO m
     , Monad m
     , MonadState State m)
     => m FH.Handle
getLocalProposalFileHandle = do
    State {..} <- get
    liftIO (createDirectoryIfMissing True localProposalRootFolder )
    newLocalOffset <- case handleMaybe of
            Nothing -> return localBlockOffset
            Just handle -> do
              _ <- liftIO (FH.hClose handle)
              return $ nextOffset localBlockOffset
    newHandle <- liftIO $ FH.openFile (localProposalRootFolder ++ show newLocalOffset ++ ".tmp") WriteMode
    put State {handleMaybe = Just newHandle,localBlockOffset = newLocalOffset,.. }
    return newHandle


