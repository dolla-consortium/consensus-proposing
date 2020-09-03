{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Packaging (packaging) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Request
import           Dolla.Consensus.Proposing.Packaging.Dependencies
import           Dolla.Consensus.Proposing.Packaging.Input
import           Dolla.Consensus.Proposing.Packaging.Output
import           Dolla.Consensus.Proposing.Packaging.Step.Serialize (serialize)
import           Dolla.Consensus.Proposing.Packaging.Step.Assign    (assign)
import           Dolla.Consensus.Proposing.Packaging.Step.Persist   (persist)
import           Dolla.Consensus.Proposing.Packaging.Step.Notify    (notify)

packaging
  :: ( MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , MonadIO m
     , S.MonadAsync m
     , MonadCatch m)
  => log (Input Request)
  -> log Output
  ->  S.SerialT m ()
packaging inputLog outputLog =
  stream infinitely inputLog
  & serialize
  & assign
  & persist
  & notify outputLog

