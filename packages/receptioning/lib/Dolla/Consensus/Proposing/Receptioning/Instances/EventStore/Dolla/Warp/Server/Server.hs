{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Server.Server 
  (runServerOnWarp) where

import           Prelude hiding (log)
import           Control.Monad
import           Control.Monad.Reader

import           Data.Coerce (coerce)
import           Data.List.NonEmpty
import           Data.Validation
import           Data.Aeson ()

import           Network.Wai.Handler.Warp hiding (Settings)
import           Servant
import           Dolla.Adapter.Servant.Wrapper

import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core
import           Dolla.Common.Dependencies.Core

import           Dolla.Consensus.Dummy.Client.Request
import qualified Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Server.Dependencies as Server
import           Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Service
import           Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Definition


runServerOnWarp :: ReaderT Server.Dependencies IO()
runServerOnWarp = do
   dependencies @ Server.Dependencies {url = URL {port},logger} <- ask
   log logger INFO "Server Up and Running"
   liftIO $ run (coerce port) $ application
                   (Proxy :: Proxy ReceptionistApi)
                   receptioningServer
                   dependencies

receptioningServer
  ::  Server.Dependencies -> Server ReceptionistApi
receptioningServer dependencies
  = checkHealthRequest dependencies
  :<|> sendClientRequests dependencies

checkHealthRequest
  :: Server.Dependencies
  -> Handler (Either (NonEmpty UnhealthyDependency) ())
checkHealthRequest dependencies = liftIO $ fmap (toEither . void) (checkHealth dependencies)

sendClientRequests
  :: Server.Dependencies
  -> NonEmpty DollaClientRequest
  -> Handler ()
sendClientRequests Server.Dependencies {logger,eventStoreClientDependencies} requests = do
  liftIO $ log logger DEBUG $ "client requests received :" ++ show requests
  liftIO $ receptioning eventStoreClientDependencies requests
