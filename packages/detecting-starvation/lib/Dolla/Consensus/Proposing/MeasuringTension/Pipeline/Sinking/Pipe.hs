{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Sinking.Pipe
  (sinking) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Reader

import qualified Streamly as S
import qualified Streamly.Prelude as S

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Sinking.Input
import           Dolla.Consensus.Proposing.MeasuringTension.Pipeline.IO.Output

import Dolla.Common.Offset (fromIntegralToOffset)

sinking
  :: ( MemoryStreamLoggable m log
     , S.MonadAsync m)
  => log Output
  -> S.SerialT m Input
  -> S.SerialT m ()
sinking outputLog inputStream
  = inputStream
      & S.indexed
      & S.map (fromIntegralToOffset . fst )
      & S.mapM (\offset -> void $ append outputLog offset LocalProposalFlowTensed)