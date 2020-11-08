{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Dolla.Consensus.Proposing.Receptioning.Instances.EventStore.Dolla.Warp.Definition
  ( ReceptionistApi
  , HealthCheckRequest
  , SendClientRequests ) where

import           Servant
import           Data.List.NonEmpty

import           Dolla.Consensus.Dummy.Client.Request

import           Dolla.Common.Dependencies.Core

type ReceptionistApi
    = HealthCheckRequest
    :<|> SendClientRequests

type HealthCheckRequest = "health" :> Get '[JSON] (Either (NonEmpty UnhealthyDependency) ())

type SendClientRequests = "consortium" :> "team" :> "sendClientRequests"
                                :> ReqBody '[JSON] (NonEmpty DollaClientRequest)
                                :> PostAccepted '[JSON] ()