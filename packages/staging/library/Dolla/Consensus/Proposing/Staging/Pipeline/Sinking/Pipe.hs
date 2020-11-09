{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Staging.Pipeline.Sinking.Pipe (sinking) where

import           Prelude hiding (log,writeFile)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S

import           Dolla.Libraries.LogEngine.LogEngine
import qualified Dolla.Consensus.Proposing.Staging.Pipeline.IO.Output as Staging
import           Dolla.Consensus.Proposing.Staging.Pipeline.Sinking.Input

sinking
  :: ( MemoryStreamLoggable m log 
     , S.MonadAsync  m )
  =>  log Staging.Output
  ->  S.SerialT m Input
  ->  S.SerialT m ()
sinking outputLog
  = S.mapM
      (\SinkNewLocalProposal {proposalId} ->
          void
          $ append
              outputLog
              proposalId
              $ Staging.LocalProposalStaged proposalId)

