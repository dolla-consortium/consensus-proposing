{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposer.Bucketizer (requestBucketizer) where

import           Prelude hiding (log,writeFile)
import           Data.Function ((&))

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Dolla.Common.Range
import           Dolla.Common.Offset

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposer.Dependencies
import           Dolla.Consensus.Proposer.Output
import           Dolla.Consensus.Request
import           Control.Monad.Reader
import           Dolla.Common.UUID.Deterministic
import           Data.ByteString hiding (length,map,append)
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified System.IO as FH
import Data.Word (Word8)
import Control.Monad.Catch (MonadCatch)
import System.Directory

import Control.Monad.State.Strict (StateT(..), get, put)
import Data.Aeson.Types (ToJSON)
import GHC.IO.IOMode


requestBucketizer
  :: ( MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , MonadIO m , S.MonadAsync m
     , MonadCatch m)
  => log Request
  -> log Output
  ->  S.SerialT m ()
requestBucketizer requestLog outputLog =
  streamWithLogItem infinitely requestLog
  & serialize
  & bucketizeAndPersist
  & S.indexed
  & S.mapM (\(blockOffset,()) -> do
      Dependencies {proposalRootFolder} <- ask
      liftIO $ renameFile
        (proposalRootFolder ++ "local/" ++ show blockOffset ++ ".tmp")
        (proposalRootFolder ++ "local/" ++ show blockOffset ++ ".proposal")
      void $ append outputLog (fromIntegralToOffset blockOffset) LocalProposalProduced)

serialize
  :: (MonadReader Dependencies m , MonadIO m , S.MonadAsync m,ToJSON a)
  => S.SerialT m a
  -> S.SerialT m Word8
serialize input =  input & S.map (unpack . getEncodedItem) & S.concatMap S.fromList

bucketizeAndPersist
  :: (MonadReader Dependencies m ,MonadIO m,Monad m) 
  => S.SerialT m Word8 
  -> S.SerialT m ()
bucketizeAndPersist inputStream = do
    let proposalSize = 100 * 1024 
    Dependencies {proposalRootFolder} <- ask
    inputStream
      & S.liftInner
      & S.chunksOf2 proposalSize getNewHandle IFH.write2
      & S.evalStateT State
                      { localProposalRootFolder = proposalRootFolder ++ "local/"
                      , handleMaybe = Nothing
                      , localBlockOffset = 0}

data State
  = State
    { localProposalRootFolder::FilePath
    , handleMaybe :: Maybe FH.Handle
    , localBlockOffset :: Offset }

getNewHandle :: (MonadIO m,Monad m) => StateT State m FH.Handle
getNewHandle = do
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


