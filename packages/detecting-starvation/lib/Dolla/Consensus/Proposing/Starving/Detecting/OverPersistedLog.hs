{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.Starving.Detecting.OverPersistedLog
  (detectingPipelineStarving) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine

import           Dolla.Consensus.Proposing.Starving.Detecting.Input
import           Dolla.Consensus.Proposing.Starving.Detecting.Output

type OverMemoryStreamProcessing m = (S.SerialT m Input ->  S.SerialT m Output)

detectingPipelineStarving
  :: ( MemoryStreamLoggable m log
     , S.MonadAsync m
     , MonadCatch m)
  => log Input
  -> log Output
  -> OverMemoryStreamProcessing m
  ->  m ()
detectingPipelineStarving inputLog outputLog overMemoryStreamProcessing
  = stream infinitely inputLog
    & overMemoryStreamProcessing
    & S.mapM_ (void . nonIdempotentAppend outputLog)
