{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Receptioning.Service.Service
  (receptioning) where

import           Data.List.NonEmpty
import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Request
import           Dolla.Libraries.LogEngine.Appendable
import           Dolla.Common.UUID.Provider
import           Dolla.Consensus.Proposing.Receptioning.Service.Output

receptioning
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
receptioning eventStoreLog requests
  = nonIdempotentAppendList  
      eventStoreLog 
      (Receptioned <$> requests)