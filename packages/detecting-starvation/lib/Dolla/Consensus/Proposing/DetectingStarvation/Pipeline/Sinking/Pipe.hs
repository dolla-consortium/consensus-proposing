{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.Sinking.Pipe
  (sinking) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Libraries.LogEngine.LogEngine

import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Output

import Dolla.Common.UUID.Deterministic (getDeterministicUUID)
import Dolla.Common.Offset (fromIntegralToOffset)

sinking
  :: (MemoryStreamLoggable m log ,S.MonadAsync m)
  => log Output
  -> S.SerialT m ()
  -> m ()
sinking outputLog inputStream
  = inputStream
      & S.indexed
      & S.map (fromIntegralToOffset . fst )
      & S.mapM(\newOffset -> do
               let itemId = getDeterministicUUID newOffset
               return NewLocalProposalAsked {..})
      & S.mapM_ (void . nonIdempotentAppend outputLog)