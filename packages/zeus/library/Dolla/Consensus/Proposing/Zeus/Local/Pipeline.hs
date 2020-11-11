{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Dolla.Consensus.Proposing.Zeus.Local.Pipeline
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

import           Dolla.Consensus.Proposing.Zeus.Local.Context
import qualified Dolla.Consensus.EventStore.Zeus.Local.Settings as EventStore
import qualified Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.Simulating as Simulating
import qualified Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.Receptionist as Receptioning
import qualified Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.Staging as Staging
import qualified Dolla.Consensus.Proposing.Zeus.Local.MicroserviceSettings.DetectingTension as Flushing


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
          stdout $ echoCommandAndInShell $ "kill $(ps -A | grep -v \"dolla-consensus-proposing-zeus\" | grep dolla-consensus-proposing- | awk '{print $1}')"  <++> "  &>/dev/null")
         (\SomeException {} -> return ())
      catch
        (do
          echo ">| Closing tabs opened for running local nodes..."
          stdout $ echoCommandAndInShell $ "sudo kill $(ps -A | grep \"multitail -s 2 -l\" | awk '{print $1}')"  <++> "  &>/dev/null")
          (\SomeException {} -> return ()))

delete
  :: MonadIO m
  => Prelude.FilePath
  -> m ()
delete rootFolder
  = do
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
                Staging.MicroServiceSettings
                { nodeId
                , executableName = "dolla-consensus-proposing-staging"
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder, fileName = "staging.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "staging.config"}
                , proposalSizeLimit
                , eventStore
                , proposalRootFolder}
            , getExecutableSettings
                Receptioning.MicroServiceSettings
                { nodeId
                , executableName = "dolla-consensus-proposing-receptioning-server"
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder, fileName = "receptioning.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "receptioning.config"}
                , eventStore
                , receptioningUrl}
            , getExecutableSettings
                Simulating.MicroserviceSettings
                { nodeId
                , executableName = "dolla-consensus-proposing-simulating"
                , stressLoad
                , eventStore
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder , fileName = "simulating.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "simulating.config"}
                , receptioningUrl}
            , getExecutableSettings
                Flushing.MicroServiceSettings
                { nodeId
                , executableName = "dolla-consensus-proposing-detecting-tension"
                , logFileLocation =  FileSystemLocation {rootFolder = logFolder, fileName = "starving-detection.log"}
                , configurationLocation =  FileSystemLocation {rootFolder = configFolder, fileName = "starving-detection.config"}
                , eventStore}
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
