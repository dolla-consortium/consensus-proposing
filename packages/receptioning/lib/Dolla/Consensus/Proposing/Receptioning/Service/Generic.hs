{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Receptioning.Service.Generic
  ( transmitRequestToProposingPackagingPipeline
  , transmitRequestsToProposingPackagingPipeline) where

import           Data.List.NonEmpty
import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Request
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Dolla.Consensus.Proposing.Receptioning.Output

transmitRequestToProposingPackagingPipeline
  :: ( Appendable clientRequest
     , Appendable consortiumRequest
     , UUIDProvider clientRequest
     , UUIDProvider consortiumRequest
     , MemoryStreamLoggable m log)
  => log (Output (Request clientRequest consortiumRequest))
  -> Request clientRequest consortiumRequest
  -> m ()
transmitRequestToProposingPackagingPipeline eventStoreLog request
  = nonIdempotentAppend 
      eventStoreLog 
      (Receptioned request)

transmitRequestsToProposingPackagingPipeline
  :: ( Appendable clientRequest
     , Appendable consortiumRequest
     , UUIDProvider clientRequest
     , UUIDProvider consortiumRequest
     , Show clientRequest
     , Show consortiumRequest
     , MemoryStreamLoggable m log)
  => log (Output (Request clientRequest consortiumRequest))
  -> NonEmpty (Request clientRequest consortiumRequest)
  -> m ()
transmitRequestsToProposingPackagingPipeline eventStoreLog requests
  = nonIdempotentAppendList  
      eventStoreLog 
      (Receptioned <$> requests)