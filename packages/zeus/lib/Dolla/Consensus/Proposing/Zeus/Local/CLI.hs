{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
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
import           System.Console.Byline hiding (askUntil,ask,askWithMenuRepeatedly)

import           Dolla.Adapter.ByLine.Wrapper (mustBeKBytesWithMax,askWithMenuRepeatedly, askUntil, renderPrefixAndSuffixForDynamicMenu)
import           Dolla.Common.Memory.Byte (mb, Byte)
import           Dolla.Common.Data.IsString ((<++>))

import           Dolla.Consensus.Proposing.Zeus.Local.Context
import           Dolla.Consensus.Common.Zeus.Local.Node

import qualified Dolla.Consensus.Proposing.Zeus.Local.Pipeline as Proposing.Pipeline
import qualified Dolla.Consensus.EventStore.Zeus.Local.NodeContainer as EventStore.NodeContainer
import qualified Dolla.Consensus.EventStore.Zeus.Local.Settings as EventStore.NodeContainer
import           Dolla.Consensus.Proposing.Simulating.StressLoad

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
    stressLoad <- liftIO $ fromJust <$> runByline (displayStressLoadMenu proposalSizeLimit)
    rootFolder <- getRootFolder
    EventStore.NodeContainer.start eventStoreSettings
    runReaderT Proposing.Pipeline.start Context {node,rootFolder,proposalSizeLimit,stressLoad}
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

displayStressLoadMenu :: MonadIO m =>  Byte -> Byline m StressLoad
displayStressLoadMenu proposalSizeLimit
   = askWithMenuRepeatedly
       stressLoadMenu
       "> please choose an option for simulating client requests (provide the index) : "
       "> please enter a valid index..."
       >>= \case
             PickOverFlowingOption _ -> return $ OverFlowing proposalSizeLimit
             PickUnderSupplyingOption _  -> return $ UnderSupplying proposalSizeLimit
             PickAlternatingStressOption _ -> return $ AlternatingStress proposalSizeLimit


stressLoadMenu :: Menu StressLoadAction
stressLoadMenu
  = renderPrefixAndSuffixForDynamicMenu
    $ menu
      [ PickOverFlowingOption       "Overflowing the pipeline with request"
      , PickUnderSupplyingOption    "Undersupplying the pipeline"
      , PickAlternatingStressOption "Alternating Overflowing/Undersupplying"]
      (\case
        PickOverFlowingOption       description -> fg green <> text description
        PickUnderSupplyingOption    description -> fg green <> text description
        PickAlternatingStressOption description -> fg green <> text description)

data StressLoadAction
  = PickOverFlowingOption Text
  | PickUnderSupplyingOption  Text
  | PickAlternatingStressOption Text