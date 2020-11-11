{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.EventStore.Zeus.Local.NodeContainer
  ( delete
  , start
  , configure)
  where

import           Control.Exception
import           Data.Coerce (coerce)
import           Data.Text hiding (empty)
import           Turtle hiding (s)

import           Dolla.Common.Data.IsString
import           Dolla.Common.Network.Core
import           Dolla.Common.System.Threading (pauseThread)
import           Dolla.Common.Time.Core
import           Dolla.Adapter.Turtle.Wrapper

import           Dolla.Consensus.Common.Zeus.Local.Node 
import           Dolla.Consensus.EventStore.Zeus.Local.Settings

delete
  :: MonadIO m
  => NodeId
  -> m ()
delete
  nodeId
  = liftIO $ catch
    (do
      echo $ ">| reset the Event Store data for " <++> coerce nodeId
      stdout $ echoCommandAndInShell $ "docker rm -f " <++> getEventStoreDockerName nodeId)
    (\SomeException {} -> return ())

start
  :: MonadIO m
  => EventStoreSettings
  -> m ()
start eventStoreSettings@EventStoreSettings {..}
  = do
  echo $ ">| starting containers for " <++> coerce nodeId
  let command = "docker"
                  <++> " run"
                  <++> " --name " <++> getEventStoreDockerName nodeId
                  <++> " -dit"
                    <++> " -p " <++> show (port projectionUrl) <++> ":2113"
                    <++> " -p " <++> show (port eventStoreUrl) <++> ":1113"
                  <++> " eventstore/eventstore:release-5.0.6 " 
  stdout $ echoCommandAndInShell command
  pauseThread $ 10 * s -- time for container to start, otherwise configuration fails :-(... Find a way to know when the container is up and running...
  configure eventStoreSettings

configure
  :: MonadIO m
  => EventStoreSettings
  -> m ()
configure settings@EventStoreSettings {nodeId} = do
  echo $ ">| Re-configure the Event Store for " <++> coerce nodeId
  enableByCategoryProjection settings

enableByCategoryProjection
  :: MonadIO m
  => EventStoreSettings
  -> m ()
enableByCategoryProjection EventStoreSettings {..} = do
  let command = "sudo" <++> " curl -i -X POST " <++> getProjectionURL
              <++> " -H 'accept:application/json' -H 'Content-Length:0' "
              <++> " -u "
                <++> fromString (unpack username )
                <++> ":"
                <++> fromString (unpack password)
--                <++> " &>/dev/null"
  stdout $ echoCommandAndInShell command

  where
    getProjectionURL =
      "'http://" <++> host projectionUrl <++> ":" <++> show (port projectionUrl) <++>
      "/projection/%24by_category/command/enable'"


getEventStoreDockerName
  :: NodeId
  -> String
getEventStoreDockerName
  NodeId {..}
  = "consensus-" ++ unNodeId