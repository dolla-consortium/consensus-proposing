{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Dolla.Consensus.Proposing.Receptioning.API.Client.Client
  ( sendProposalRequest
  , sendProposalRequests
  , sendHealthCheckRequest)
  where


import qualified Servant.Client.Streaming as S
import           Servant
import           Dolla.Consensus.Proposing.Receptioning.API.Definition
import           Dolla.Consensus.Dummy.Client.Request
import           Data.Validation
import           Dolla.Common.Dependencies.Core
import           Data.List.NonEmpty
import           Servant.Client
import           Network.HTTP.Client (Manager)
import           Control.Monad.IO.Class

sendProposalRequest
  :: Manager
  -> BaseUrl
  -> DollaClientRequest
  -> IO (Either String ())
sendProposalRequest httpClientManager url proposalRequest =
  S.withClientM
    (sendProposalRequestCall proposalRequest)
    (S.mkClientEnv httpClientManager url)
    (either 
      (return . Left. show)
      (return . Right))
  where
    sendProposalRequestCall :: DollaClientRequest -> S.ClientM ()
    sendProposalRequestCall = S.client (Proxy :: Proxy SendClientRequest )

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




