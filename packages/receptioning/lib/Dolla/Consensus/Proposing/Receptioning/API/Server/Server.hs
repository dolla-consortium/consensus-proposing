{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Receptioning.API.Server.Server (execute) where

import           Prelude hiding (log)
import           Data.List.NonEmpty
import           Data.Validation
import           Data.Aeson ()
import           Servant
import           Network.Wai.Handler.Warp hiding (Settings)
import           Dolla.Adapter.Servant.Wrapper

import           Dolla.Common.Logging.Core
import           Dolla.Common.Network.Core
import           Dolla.Common.Dependencies.Core

import           Dolla.Consensus.Dummy.Client.Request
import qualified Dolla.Consensus.Proposing.Receptioning.API.Server.Settings as Server
import qualified Dolla.Consensus.Proposing.Receptioning.API.Server.Dependencies     as Server
import qualified Dolla.Consensus.Proposing.Receptioning.Service.OverDolla as DollaService
import           Dolla.Consensus.Proposing.Receptioning.API.Definition
import           Control.Monad
import           Control.Monad.Reader

import           Dolla.Common.Executable.Executable
import           Data.Coerce (coerce)

execute :: IO ()
execute = executeMicroservice (\Server.Settings {logger} -> logger) runServerOnWarp

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
  :<|> sendClientRequest dependencies
  :<|> sendClientRequests dependencies

checkHealthRequest
  :: Server.Dependencies
  -> Handler (Either (NonEmpty UnhealthyDependency) ())
checkHealthRequest dependencies = liftIO $ fmap (toEither . void) (checkHealth dependencies)

sendClientRequest
  :: Server.Dependencies
  -> DollaClientRequest
  -> Handler ()
sendClientRequest Server.Dependencies {logger,eventStoreClientDependencies} proposalRequest = do
  liftIO $ log logger DEBUG $ "client request received :" ++ show proposalRequest
  liftIO $ DollaService.persistClientRequest
                            eventStoreClientDependencies
                            proposalRequest
                            
sendClientRequests
  :: Server.Dependencies
  -> NonEmpty DollaClientRequest
  -> Handler ()
sendClientRequests Server.Dependencies {logger,eventStoreClientDependencies} proposalRequests = do
  liftIO $ log logger DEBUG $ "client requests received :" ++ show proposalRequests
  liftIO $ DollaService.persistClientRequests
                            eventStoreClientDependencies
                            proposalRequests
