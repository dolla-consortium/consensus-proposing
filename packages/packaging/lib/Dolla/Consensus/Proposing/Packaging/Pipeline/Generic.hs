{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Packaging.Pipeline.Generic
  (packaging) 
  where

import           Prelude 
import           Data.Aeson.Types (ToJSON,FromJSON)

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)


import qualified Streamly as S

import           Dolla.Libraries.LogEngine.LogEngine (MemoryStreamLoggable (..))
import           Dolla.Common.Range (infinitely)

import           Dolla.Consensus.Proposing.Packaging.Dependencies

import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output

import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Pipe  (serializing)
import           Dolla.Consensus.Proposing.Packaging.Pipes.NonEmptying.Pipe  (nonEmptying)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Pipe      (capping)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Pipe   (persisting)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Notifying.Pipe    (notifying)

import           Dolla.Common.Pipeline.Weldable ((~>),(.~>))
import           Dolla.Consensus.Proposing.Packaging.Pipeline.Welding.BluePrint ()

-- | Packaging Pipeline expressed in its most polymorphic way :
--  - Log Engine Agnostic
--  - Request Agnostic
packaging
  :: ( ToJSON request
     , FromJSON request
     , Show request
     , MonadCatch m
     , MonadReader Dependencies m
     , S.MonadAsync m
     , MemoryStreamLoggable m log
     )
  => log (Input request)
  -> log Output
  -> S.SerialT m ()
packaging inputLog outputLog
  = do
  Dependencies {proposalSizeLimit,proposalRootFolder} <- ask
  stream infinitely inputLog
     ~> serializing
    .~> nonEmptying
    .~> capping proposalSizeLimit
    .~> persisting proposalRootFolder
    .~> notifying outputLog





