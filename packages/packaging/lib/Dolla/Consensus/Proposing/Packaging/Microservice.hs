{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Packaging.Microservice (execute) where

import           Prelude hiding (log)
import           Control.Monad.Reader

import           Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SIP

import           Dolla.Common.Logging.Core
import           Dolla.Common.Executable.Executable

import           Dolla.Consensus.Proposing.Packaging.Pipeline.OverEventStoreAndDolla (packaging)
import           Dolla.Consensus.Proposing.Packaging.Settings
import           Dolla.Consensus.Proposing.Packaging.Dependencies

execute :: IO ()
execute
  = executeMicroservice
      (\Settings {logger} -> logger)
      executePipeline
  where 
    executePipeline :: ReaderT Dependencies IO ()
    executePipeline = do
      dependencies@Dependencies {logger} <- ask
      log logger INFO "Pipeline Starting"
      lift $ drain $ SIP.runReaderT dependencies packaging
      log logger INFO "End Of Pipeline"

