{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Persist (persist) where

import           Prelude hiding (log,writeFile)

import           GHC.IO.IOMode
import           System.Directory
import qualified System.IO as FH
import           Data.Function ((&))
import           Control.Monad.Reader
import           Control.Monad.State (MonadState, put, get)

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Dolla.Common.Streamly as S (groupsBy2,lmap2)

import           Dolla.Common.Offset

import           Dolla.Consensus.Proposing.Packaging.Dependencies

import           Dolla.Consensus.Proposing.Packaging.Step.Assign (sameAssignment, Assignment (..))


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
          sameAssignment
          getLocalProposalFileHandle
          (S.lmap2 requestByteChunk IFH.write2)
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


