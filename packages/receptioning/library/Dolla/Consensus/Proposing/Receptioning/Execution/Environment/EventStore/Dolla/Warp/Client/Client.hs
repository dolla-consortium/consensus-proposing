{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Client.Client
  ( sendProposalRequests
  , sendHealthCheckRequest)
  where

import           Control.Monad.IO.Class
import           Data.Validation
import           Data.List.NonEmpty

import           Network.HTTP.Client (Manager)

import qualified Servant.Client.Streaming as S

import           Servant
import           Servant.Client

import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Common.Dependencies.Core
import           Dolla.Consensus.Proposing.Receptioning.Execution.Environment.EventStore.Dolla.Warp.Definition

sendProposalRequests
  :: MonadIO m
  => Manager
  -> BaseUrl
  -> NonEmpty DollaClientRequest -- compressed / cut them into unit compress pieces
  -> m (Either String ())
sendProposalRequests httpClientManager url proposalRequests =
  liftIO $ S.withClientM
    (sendProposalRequestsCall proposalRequests)
    (S.mkClientEnv httpClientManager url)
    (either
      (return . Left. show)
      (return . Right))
  where
    sendProposalRequestsCall :: NonEmpty DollaClientRequest -> S.ClientM ()
    sendProposalRequestsCall = S.client (Proxy :: Proxy SendClientRequests )

sendHealthCheckRequest
  :: Manager
  -> BaseUrl
  -> IO (Validation (NonEmpty UnhealthyDependency) ())
sendHealthCheckRequest httpClientManager url  =
  S.withClientM
    sendHealthCheckRequestCall
    (S.mkClientEnv httpClientManager url)
    (either
      (\servantError ->
        return . fromEither $ Left $ pure $ UnhealthyDependency
               { name = "receptioning.server"
               , unhealthyReason = show servantError} )
      (return . fromEither))
  where
    sendHealthCheckRequestCall
      :: S.ClientM (Either (NonEmpty UnhealthyDependency) ())
    sendHealthCheckRequestCall
      = S.client (Proxy :: Proxy HealthCheckRequest )




