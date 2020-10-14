{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Dolla.Consensus.Proposing.Receptioning.API.Definition
  ( ReceptionistApi
  , HealthCheckRequest
  , SendClientRequest
  , SendClientRequests ) where

import           Servant
import           Dolla.Consensus.Dummy.Client.Request
import           Data.List.NonEmpty
import           Dolla.Common.Dependencies.Core

type ReceptionistApi
    = HealthCheckRequest
    :<|> SendClientRequest
    :<|> SendClientRequests


type HealthCheckRequest = "health" :> Get '[JSON] (Either (NonEmpty UnhealthyDependency) ())

type SendClientRequest = "consortium" :> "team" :> "sendClientRequest"
                                :> ReqBody '[JSON] DollaClientRequest
                                :> PostAccepted '[JSON] ()

type SendClientRequests = "consortium" :> "team" :> "sendClientRequests"
                                :> ReqBody '[JSON] (NonEmpty DollaClientRequest)
                                :> PostAccepted '[JSON] ()