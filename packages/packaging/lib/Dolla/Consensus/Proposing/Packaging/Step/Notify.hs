{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Notify (notify) where

import           Prelude hiding (log,writeFile)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S

import           Dolla.Common.Offset

import           Dolla.Libraries.LogEngine.LogEngine
import           Dolla.Consensus.Proposing.Packaging.Output

notify
  :: ( MemoryStreamLoggable m log 
     , S.MonadAsync  m )
  =>  log Output
  ->  S.SerialT m Offset
  ->  S.SerialT m ()
notify outputLog = S.mapM (\localOffsetPersisted -> void $ append outputLog localOffsetPersisted LocalProposalProduced)

