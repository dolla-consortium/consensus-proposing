{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Packaging.OverMemoryStream 
  (packaging) 
  where

import           Prelude 
import           Data.Aeson.Types (ToJSON,FromJSON)

import           Control.Monad.Reader
import           Control.Monad.Catch (MonadCatch)


import qualified Streamly as S
import           Dolla.Adapter.Streamly.Connectable ((~>),(.~>))

import           Dolla.Libraries.LogEngine.LogEngine (MemoryStreamLoggable (..))
import           Dolla.Common.Range (infinitely)

import           Dolla.Consensus.Proposing.Packaging.Dependencies

import           Dolla.Consensus.Proposing.Packaging.Input
import           Dolla.Consensus.Proposing.Packaging.Output

import           Dolla.Consensus.Proposing.Packaging.Serializing.StreamProcessing  (serializing)
import           Dolla.Consensus.Proposing.Packaging.NonEmptying.StreamProcessing  (nonEmptying)
import           Dolla.Consensus.Proposing.Packaging.Capping.StreamProcessing      (capping)
import           Dolla.Consensus.Proposing.Packaging.Persisting.StreamProcessing   (persisting)
import           Dolla.Consensus.Proposing.Packaging.Notifying.StreamProcessing    (notifying)

import           Dolla.Consensus.Proposing.Packaging.Connecting ()

packaging
  :: ( ToJSON request
     , FromJSON request
     , Show request
     , MemoryStreamLoggable m log
     , MonadReader Dependencies m
     , S.MonadAsync m
     , MonadCatch m)
  => log (Input request)
  -> log Output
  -> S.SerialT m ()
packaging inputLog outputLog
  = do
  Dependencies {proposalSizeLimit,proposalRootFolder}<- ask
  stream infinitely inputLog
     ~> serializing
    .~> nonEmptying
    .~> capping proposalSizeLimit
    .~> persisting proposalRootFolder
    .~> notifying outputLog





