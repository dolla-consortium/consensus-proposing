{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Packaging.Pipeline.Sinking.Pipe (sinking) where

import           Prelude hiding (log,writeFile)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S

import           Dolla.Libraries.LogEngine.LogEngine
import qualified Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output as Packaging
import           Dolla.Consensus.Proposing.Packaging.Pipeline.Sinking.Input

sinking
  :: ( MemoryStreamLoggable m log 
     , S.MonadAsync  m )
  =>  log Packaging.Output
  ->  S.SerialT m Input
  ->  S.SerialT m ()
sinking outputLog
  = S.mapM
      (\SinkNewLocalProposal {proposalId} ->
          void
          $ append
              outputLog
              proposalId
              $ Packaging.LocalProposalPackaged proposalId)

