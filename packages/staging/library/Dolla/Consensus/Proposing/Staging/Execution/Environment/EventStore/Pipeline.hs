{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Pipeline (staging) where

import           Prelude hiding (log)
import           Data.Data
import           Data.Proxy
import           Data.Aeson.Types (ToJSON,FromJSON)
import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import qualified Dolla.Consensus.Proposing.Staging.Pipeline.Pipeline as Generic
import           Dolla.Consensus.Proposing.Staging.Execution.Environment.EventStore.Dependencies
import           Dolla.Consensus.Log.EventStoreLog
import           Dolla.Consensus.Log.LogNameIndex

import           Dolla.Consensus.Proposing.Staging.Pipeline.IO.Input
import           Dolla.Libraries.LogEngine.Instances.EventStore.EventStoreLog

staging
  :: ( ToJSON request
     , FromJSON request
     , Show request
     , MonadReader Dependencies m
     , S.MonadAsync m
     , MonadCatch m)
  => Proxy request
  -> S.SerialT m ()
staging proxy = do
  Dependencies {eventStoreClient,proposalRootFolder,proposalSizeLimit} <- ask
  Generic.staging
        proposalRootFolder
        proposalSizeLimit
        (asProxyTypeOf (getEventStoreLog eventStoreClient ProposingStagingInputLog) (getProxyLogInput proxy))
        (getEventStoreLog eventStoreClient ProposingStagingOutputLog)

getProxyLogInput :: Proxy  request  -> Proxy (EventStoreLog (Input request))
getProxyLogInput _ = Proxy 

