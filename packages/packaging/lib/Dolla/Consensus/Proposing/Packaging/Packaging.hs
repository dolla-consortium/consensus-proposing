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
import           Dolla.Consensus.Proposing.Packaging.Dependencies
import           Dolla.Consensus.Proposing.Packaging.Output
import           Dolla.Consensus.Request

import           Dolla.Consensus.Proposing.Packaging.Step.Notify (notify)
import           Dolla.Consensus.Proposing.Packaging.Step.Serialize (serialize)
import           Dolla.Consensus.Proposing.Packaging.Step.Assign (assign)
import           Dolla.Consensus.Proposing.Packaging.Step.Persist (persist)

packaging
  :: ( MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , MonadIO m
     , S.MonadAsync m
     , MonadCatch m)
  => log Request
  -> log Output
  ->  S.SerialT m ()
packaging requestLog outputLog =
  streamWithLogItem infinitely requestLog
  & serialize
  & assign
  & persist
  & notify outputLog

