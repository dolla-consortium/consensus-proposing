{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Proposer.Zeus.Local.Pipeline
  ( start
  , stop
  , delete
  )
  where

import           Control.Monad.Reader
import           Control.Exception
import           Data.Coerce (coerce)

import           Text.InterpolatedString.Perl6 (qc)
import           Turtle

import           Dolla.Adapter.Turtle.Wrapper
import           Dolla.Common.Data.IsString

import           Dolla.Consensus.Common.Zeus.Haskell.ExecutableSettings
import           Dolla.Consensus.Common.Zeus.Local.Node
import qualified Dolla.Consensus.Common.Zeus.Local.NodeEntryPoints as NodeEntryPoints

import           Dolla.Consensus.Proposer.Zeus.Local.Context
import qualified Dolla.Consensus.EventStore.Zeus.Local.Settings as EventStore
import qualified Dolla.Consensus.Proposer.Zeus.Local.MicroserviceSettings.Bucketizer as Bucketizer
import qualified Dolla.Consensus.Proposer.Zeus.Local.MicroserviceSettings.Receptionist as Receptionist
import qualified Dolla.Consensus.Proposer.Zeus.Local.MicroserviceSettings.ClientSimulator as ClientSimulator
start
  :: MonadIO m
  => ReaderT Context m ()
start = stop
      >>  getProposerMicroservicesSettings
      >>= \microservices -> sequence (saveConfigurationsAndCreateLogFolder <$> microservices)
      >>= savePipelineBootstrapScript
      >>= lift . openANewTabAndRunProcess

stop
  :: MonadIO m => m ()
stop
  = liftIO
      (do
      catch
        (do
          echo ">| Killing proposer microservices badly closed (issue with multitail...)"
          stdout $ echoCommandAndInShell $ "kill $(ps -A | grep -v \"dolla-consensus-proposer-zeus\" | grep dolla-consensus-proposer- | awk '{print $1}')"  <++> "  &>/dev/null")
         (\SomeException {} -> return ())
      catch
        (do
          echo ">| Closing tabs opened for running local nodes..."
          stdout $ echoCommandAndInShell $ "sudo kill $(ps -A | grep \"multitail -s 2 -l\" | awk '{print $1}')"  <++> "  &>/dev/null")
          (\SomeException {} -> return ()))

delete
  :: MonadIO m
  => ReaderT Context m ()
delete
  = do
  Context {rootFolder} <- ask
  echo ">| Deleting proposals recorded on the file system"
  stdout $ echoCommandAndInShell $ "rm -rf "<++> rootFolder <++> "*"


getProposerMicroservicesSettings
  :: Monad m
  => ReaderT Context m [ExecutableSettings]
getProposerMicroservicesSettings
  = do
    Context {..} <- ask
    let nodeFolder = getNodeFolder rootFolder node
        NodeEntryPoints.EntryPointsSettings {..} = NodeEntryPoints.getLocalConfiguration node
        eventStore = EventStore.getLocalSettings node
        proposalRootFolder = nodeFolder ++ "proposals/"
        configFolder = nodeFolder ++ "configuration/Proposer/"
        logFolder = nodeFolder ++ "log/"
    return [ getExecutableSettings
                Bucketizer.MicroServiceSettings
                { nodeId
                , executableName = "dolla-consensus-proposer-bucketizer"
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder, fileName = "bucketizer.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "bucketizer.config"}
                , eventStore
                , proposalRootFolder}
            , getExecutableSettings
                Receptionist.MicroServiceSettings
                { nodeId
                , executableName = "dolla-consensus-proposer-receptionist"
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder, fileName = "receptionist.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "receptionist.config"}
                , eventStore
                , receptionistUrl}
            , getExecutableSettings
                ClientSimulator.MicroserviceSettings
                { nodeId
                , executableName = "dolla-consensus-proposer-client-simulator"
                , logFileLocation =  FileSystemLocation {rootFolder , fileName = "client-simulator.log"}
                , configurationLocation =  FileSystemLocation {rootFolder, fileName = "client-simulator.config"}
                , receptionistUrl}
             ]


savePipelineBootstrapScript
  :: MonadIO m
  => [ExecutableSettings]
  -> ReaderT Context m String
savePipelineBootstrapScript microservices
  = ask >>= \Context {node = node@Node {..},rootFolder} -> do
      let nodeFolder = getNodeFolder rootFolder node
          nodeCommand = mconcat ((\microserviceSetting ->
                        [qc|-l "{getRunCommand microserviceSetting} 2>&1 | tee {getLogFilePath microserviceSetting}" \\|] <++> "\n" )
                        <$> microservices )
          tabName :: String = coerce nodeId
          bootStrapFileName = nodeFolder ++ "bootstrap.sh"
          nodeScript = [qc|#!/bin/bash |] <++> "\n" <++>
                       [qc|DISABLE_AUTO_TITLE="true" && \\|] <++> "\n" <++>
                       [qc|echo -ne "\033]0;{tabName}\007" && \\|] <++> "\n" <++>
                       [qc|cd {rootFolder} && \\|] <++> "\n" <++>
                       [qc|multitail -s 2 \\|] <++> "\n" <++> nodeCommand <++>
                       [qc|;exit;|]
      liftIO $ writeFile bootStrapFileName nodeScript
      stdout $ echoCommandAndInShell $ "chmod 777 " <++> bootStrapFileName
      return bootStrapFileName
