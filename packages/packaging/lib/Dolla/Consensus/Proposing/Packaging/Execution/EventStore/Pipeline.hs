{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Packaging.Execution.EventStore.Pipeline (packaging) where

import           Prelude hiding (log)
import           Data.Data
import           Data.Proxy
import           Data.Aeson.Types (ToJSON,FromJSON)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import qualified Dolla.Consensus.Proposing.Packaging.Pipeline.Pipeline as Generic
import           Dolla.Consensus.Proposing.Packaging.Execution.EventStore.Dependencies
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input
import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog

packaging
  :: ( ToJSON request
     , FromJSON request
     , Show request
     , MonadReader Dependencies m
     , S.MonadAsync m
     , MonadCatch m)
  => Proxy request
  -> S.SerialT m ()
packaging proxy = do
  Dependencies {eventStoreClient,proposalRootFolder,proposalSizeLimit} <- ask
  Generic.packaging
        proposalRootFolder
        proposalSizeLimit
        (asProxyTypeOf (getEventStoreLog eventStoreClient LocalRequestLog) (getProxyLogInput proxy))
        (getEventStoreLog eventStoreClient ProposingPackagingOutputLog)

getProxyLogInput :: Proxy  request  -> Proxy (EventStoreLog (Input request))
getProxyLogInput _ = Proxy 

