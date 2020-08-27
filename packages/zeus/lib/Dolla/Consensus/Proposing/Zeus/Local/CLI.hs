{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Dolla.Consensus.Proposing.Zeus.Local.CLI
  ( start
  , stop
  , delete
  )
  where

import           Prelude hiding (writeFile)
import           System.Directory (getCurrentDirectory)
import           Control.Monad.Reader

import           Turtle hiding (text)

import           Dolla.Consensus.Proposing.Zeus.Local.Context
import           Dolla.Consensus.Common.Zeus.Local.Node

import qualified Dolla.Consensus.Proposing.Zeus.Local.Pipeline as Proposing.Pipeline
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
    runReaderT Proposing.Pipeline.start Context {..}
    echo "------------------------------"

stop :: MonadIO m => m ()
stop = do
  echo "------------------------------"
  echo "* Stopping Local Proposer * "
  echo "------------------------------"
  Proposing.Pipeline.stop
  echo "------------------------------"
  
delete :: MonadIO m => m ()
delete
  = do
  echo   "-------------------------------------------"
  echo   " - Delete Local Proposing Pipeline Data"
  echo   "-------------------------------------------"
  let node@Node {..} = getNode 0
  rootFolder <- getRootFolder
  EventStore.NodeContainer.delete nodeId
  runReaderT Proposing.Pipeline.delete Context {..}


getRootFolder :: MonadIO m => m Prelude.FilePath
getRootFolder = liftIO $ (++ "/output/") <$> getCurrentDirectory

