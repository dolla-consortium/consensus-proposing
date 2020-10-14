{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Packaging.OverDolla (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader
import           Data.Data

import           Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SIP

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import qualified Dolla.Consensus.Proposing.Packaging.OverEventStore as OverEventStore
import           Dolla.Consensus.Proposing.Packaging.Settings
import           Dolla.Consensus.Proposing.Packaging.Dependencies

import           Dolla.Consensus.Request
import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Consensus.Consortium.Request


execute :: IO ()
execute = executeMicroservice (\Settings {logger} -> logger) start

start :: ReaderT Dependencies IO ()
start = do
  dependencies@Dependencies {logger} <- ask
  log logger INFO "Proposing Packaging pipeline running"
  lift $ drain
    $ SIP.runReaderT
        dependencies
        (OverEventStore.packaging getSpecificRequestTypeForDolla)
  log logger INFO "Proposing Packaging pipeline running"

getSpecificRequestTypeForDolla :: Proxy (Request DollaClientRequest ConsortiumRequest)
getSpecificRequestTypeForDolla = Proxy