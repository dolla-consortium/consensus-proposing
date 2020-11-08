{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.Pipeline
  (detectingStarvation) where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import           Control.Monad.Catch (MonadCatch)

import qualified Streamly as S

import           Dolla.Common.Range

import           Dolla.Libraries.LogEngine.LogEngine


import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Output
import qualified Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.Pipe as Pipe
import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.Sinking.Pipe (sinking)

detectingStarvation
  :: ( MemoryStreamLoggable m log
     , S.MonadAsync m
     , MonadCatch m)
  => log Input 
  -> log Output
  ->  m ()
detectingStarvation inputLog outputLog 
  = stream infinitely inputLog -- sourcing
    & Pipe.detectStarvation
    & sinking outputLog
