{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Packaging.Notifying.StreamProcessing (notifying) where

import           Prelude hiding (log,writeFile)
import           Control.Monad.Reader

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S

import           Dolla.Libraries.LogEngine.LogEngine
import qualified Dolla.Consensus.Proposing.Packaging.Output as Packaging
import           Dolla.Consensus.Proposing.Packaging.Notifying.Input

notifying
  :: ( MemoryStreamLoggable m log 
     , S.MonadAsync  m )
  =>  log Packaging.Output
  ->  S.SerialT m Input
  ->  S.SerialT m ()
notifying outputLog
  = S.mapM
      (\LocalProposalProduced {proposalId} ->
          void
          $ append
              outputLog
              proposalId
              $ Packaging.LocalProposalProduced proposalId)

