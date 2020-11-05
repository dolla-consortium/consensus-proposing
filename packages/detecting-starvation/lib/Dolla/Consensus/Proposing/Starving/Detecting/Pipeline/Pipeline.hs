{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.Pipeline
  (detectingStarvation) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.StreamProcessing (detectStarvation )

import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Output

import Dolla.Common.UUID.Deterministic (getDeterministicUUID)
import Dolla.Common.Offset (fromIntegralToOffset)

detectingStarvation
  :: ( MemoryStreamLoggable m log
     , S.MonadAsync m
     , MonadCatch m)
  => log Input 
  -> log Output
  ->  m ()
detectingStarvation inputLog outputLog 
  = stream infinitely inputLog 
    & detectStarvation
    & appendDetection outputLog

appendDetection
  :: (MemoryStreamLoggable m log ,S.MonadAsync m)
  => log Output
  -> S.SerialT m ()
  -> m ()
appendDetection outputLog inputStream
  = inputStream
      & S.indexed
      & S.map (fromIntegralToOffset . fst )
      & S.mapM(\starvingOffset -> do
               let itemId = getDeterministicUUID starvingOffset
               return LocalProposalStarvationDetected {..})
      & S.mapM_ (void . nonIdempotentAppend outputLog)