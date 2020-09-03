{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Receptioning.Service.Generic
  ( transmitClientRequestToProposing
  , transmitClientRequestsToProposing) where


import           Dolla.Consensus.Dummy.Client.Request
import           Data.List.NonEmpty
import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Request
import           Dolla.Consensus.Proposing.Packaging.Input as Proposing.Packaging

transmitClientRequestToProposing
  :: MemoryStreamLoggable IO log
  => log (Proposing.Packaging.Input Request)
  -> ClientRequest
  -> IO ()
transmitClientRequestToProposing eLog clientRequest = nonIdempotentAppend eLog $ RequestData (ClientReq clientRequest)

transmitClientRequestsToProposing
  :: MemoryStreamLoggable IO log
  => log (Proposing.Packaging.Input Request)
  -> NonEmpty ClientRequest
  -> IO ()
transmitClientRequestsToProposing eLog clientRequests = nonIdempotentAppendList eLog $ RequestData . ClientReq <$> clientRequests 