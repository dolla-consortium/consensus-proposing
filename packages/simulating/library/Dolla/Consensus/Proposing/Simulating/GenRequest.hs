{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Dolla.Consensus.Proposing.Simulating.GenRequest
  (generateRequests)
  where

import           Prelude hiding (log,mapM_,repeat,take)
import           Data.List.NonEmpty (NonEmpty (..),fromList)
import           Data.ByteString (unpack)
import           Data.UUID ()

import           Control.Monad.Reader
import           System.Random

import           Dolla.Consensus.Dummy.Client.Request
import           Dolla.Consensus.Consortium.Request
import           Dolla.Common.Memory.Byte (Byte)
import           Dolla.Common.UUID.Deterministic
import           Dolla.Consensus.Request

generateRequests
  :: ( MonadIO m)
  => Byte
  -> m (NonEmpty DollaClientRequest)
generateRequests memorySize
  = do
    commandId <- liftIO randomIO
    source <- liftIO randomIO
    destination <- liftIO randomIO
    let request = SpendMoney {amount = 100, ..}
        requestFormatInProposal = (ClientReq request :: Request DollaClientRequest ConsortiumRequest)
        requestSizeInProposal :: Byte =  (fromIntegral . length . unpack . getEncodedItem)  requestFormatInProposal
        nbRequestToReplicate :: Int = fromIntegral (memorySize `div` requestSizeInProposal)
    return $ fromList $ replicate nbRequestToReplicate request
