{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Dolla.Consensus.Proposer.Zeus.Local.CLI
  ( start
  , stop
  , delete
  )
  where

import           Prelude hiding (writeFile)
import           System.Directory (getCurrentDirectory)
import           Control.Monad.Reader

import           Turtle hiding (text)

import           Dolla.Consensus.Proposer.Zeus.Local.Context
import           Dolla.Consensus.Common.Zeus.Local.Node

import qualified Dolla.Consensus.Proposer.Zeus.Local.Pipeline as Proposer.Pipeline
import qualified Dolla.Consensus.EventStore.Zeus.Local.NodeContainer as EventStore.NodeContainer
import qualified Dolla.Consensus.EventStore.Zeus.Local.Settings as EventStore.NodeContainer


start :: MonadIO m => m ()
start
  = do
    echo "------------------------------"
    echo "* Starting Local Proposer * "
    echo "------------------------------"
    let node@Node {..} = getNode 0
        eventStoreSettings = EventStore.NodeContainer.getLocalSettings node

    rootFolder <- getRootFolder
    EventStore.NodeContainer.start eventStoreSettings
    runReaderT Proposer.Pipeline.start Context {..}
    echo "------------------------------"

stop :: MonadIO m => m ()
stop = do
  echo "------------------------------"
  echo "* Stopping Local Proposer * "
  echo "------------------------------"
  Proposer.Pipeline.stop
  echo "------------------------------"
  
delete :: MonadIO m => m ()
delete
  = do
  echo   "-------------------------------------------"
  echo   " - Delete Local Proposer Pipeline Data"
  echo   "-------------------------------------------"
  let node@Node {..} = getNode 0
  rootFolder <- getRootFolder
  EventStore.NodeContainer.delete nodeId
  runReaderT Proposer.Pipeline.delete Context {..}


getRootFolder :: MonadIO m => m Prelude.FilePath
getRootFolder = liftIO $ (++ "/output/") <$> getCurrentDirectory

