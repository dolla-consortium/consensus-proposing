{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposer.Receptionist.Service.Generic
  ( persistClientRequest
  , persistClientRequests) where


import           Dolla.Consensus.Dummy.Client.Request
import           Data.List.NonEmpty
import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Request

persistClientRequest
  :: MemoryStreamLoggable IO log
  => log Request
  -> ClientRequest
  -> IO ()
persistClientRequest eLog clientRequest = nonIdempotentAppend eLog $ ClientReq clientRequest

persistClientRequests
  :: MemoryStreamLoggable IO log
  => log Request
  -> NonEmpty ClientRequest
  -> IO ()
persistClientRequests eLog clientRequests = nonIdempotentAppendList eLog $ ClientReq <$> clientRequests