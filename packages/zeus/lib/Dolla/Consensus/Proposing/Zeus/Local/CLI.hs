{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Zeus.Local.CLI
  ( start
  , stop
  , delete
  )
  where

import           Prelude hiding (writeFile)
import           System.Directory (getCurrentDirectory)
import           Data.Maybe (fromJust)
import           Control.Monad.Reader

import           Turtle hiding (text)
import           System.Console.Byline hiding (askUntil,ask)

import           Dolla.Adapter.ByLine.Wrapper (mustBeKBytesWithMax, askUntil)
import           Dolla.Common.Memory.Byte (mb)
import           Dolla.Common.Data.IsString ((<++>))

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
    proposalSizeLimit
        <- liftIO $ fromJust <$> runByline (askUntil
            "Enter the proposal size limit size in kilobytes ? [max 10 mb] < : "
            $ mustBeKBytesWithMax (10 * mb))
    echo ("> Proposal limit size =  " <++> show proposalSizeLimit)
    rootFolder <- getRootFolder
    EventStore.NodeContainer.start eventStoreSettings
    runReaderT Proposing.Pipeline.start Context {node,rootFolder,proposalSizeLimit}
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
  let Node {..} = getNode 0
  rootFolder <- getRootFolder
  EventStore.NodeContainer.delete nodeId
  Proposing.Pipeline.delete rootFolder


getRootFolder :: MonadIO m => m Prelude.FilePath
getRootFolder = liftIO $ (++ "/output/") <$> getCurrentDirectory

