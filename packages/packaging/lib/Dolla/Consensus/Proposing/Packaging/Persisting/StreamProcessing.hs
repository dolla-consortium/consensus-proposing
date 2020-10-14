{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Packaging.Persisting.StreamProcessing
  ( persisting
  ) where

import           Prelude hiding (log,writeFile)

import           GHC.IO.IOMode
import           System.Directory
import qualified System.IO as FH
import           Foreign.Storable (Storable(..))

import           Data.Function ((&))

import           Control.Monad.Reader
import           Control.Monad.State (MonadState, put, get)

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import qualified Streamly.Internal.Prelude as S

import qualified Dolla.Common.Streamly as S (groupsBy2,lmapM2, writeChunks2)
import           Dolla.Common.Offset
import           Dolla.Consensus.Proposal.Persistence
import           Dolla.Consensus.Proposing.Packaging.Persisting.Input
import           Dolla.Consensus.Proposing.Packaging.Persisting.Output

persisting
  :: (S.MonadAsync m ,Storable a)
  => ProposalRootFolder
  -> S.SerialT m (Input [a])
  -> S.SerialT m Output
persisting proposalRootFolder inputStream =
    inputStream
      & S.liftInner
      & S.groupsBy2
          sameLocalProposal
          (getLocalProposalFileHandle proposalRootFolder)
          (S.lmapM2 (\case
               CommitProposal  -> return []
               PersistRequest a -> do
                  get
                    >>= \case
                    CurrentProposalHandle {..} -> return CurrentProposalHandle {nbRequestSaved = nbRequestSaved + 1,.. }
                    Initial {..} -> return Initial {nbRequestSaved = nbRequestSaved + 1,.. }
                    >>= put
                  return a) S.writeChunks2)
      & S.indexed
      & S.concatMap (\(blockOffset,()) -> do
          state <- get
          liftIO (FH.hClose $ handle state)
          case nbRequestSaved state of
            0 -> S.nil
            _ -> S.yieldM $ LocalProposalPersisted
                            <$> transactLocalProposalCreation
                                  proposalRootFolder
                                  (fromIntegralToOffset blockOffset))
      & S.evalStateT Initial {  initialLocalBlockOffset = 0 , nbRequestSaved = 0}


data State
  = Initial {  initialLocalBlockOffset :: Offset , nbRequestSaved :: Integer }
  | CurrentProposalHandle {  localBlockOffset :: Offset , handle :: FH.Handle , nbRequestSaved :: Integer }
  deriving Show

getLocalProposalFileHandle
  :: ( MonadIO m
     , MonadState State m)
     => ProposalRootFolder
     -> m FH.Handle
getLocalProposalFileHandle proposalRootFolder
  = get >>= \case
    Initial {..}
      -> do
      liftIO (createDirectoryIfMissing True (getLocalProposalFolder proposalRootFolder))
      handle <- liftIO $ FH.openFile (getLocalProposalTemporaryFile proposalRootFolder initialLocalBlockOffset) WriteMode
      put CurrentProposalHandle {handle,localBlockOffset = initialLocalBlockOffset ,nbRequestSaved = 0}
      return handle
    CurrentProposalHandle {localBlockOffset = previousLocalBlockOffset}
      -> do
      let localBlockOffset = nextOffset previousLocalBlockOffset
      handle <- liftIO $ FH.openFile (getLocalProposalTemporaryFile proposalRootFolder localBlockOffset) WriteMode
      put CurrentProposalHandle {handle,localBlockOffset,nbRequestSaved = 0}
      return handle


sameLocalProposal
  :: Input a
  -> Input a
  -> Bool
sameLocalProposal CommitProposal {}  _ = False
sameLocalProposal  _ _        = True
