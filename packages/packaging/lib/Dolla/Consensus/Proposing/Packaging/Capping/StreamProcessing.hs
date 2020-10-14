{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Packaging.Capping.StreamProcessing
  ( capping)
  where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))


import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Streamly.Internal.Data.Fold.Types

import           Dolla.Common.Memory.Byte (Byte)

import           Dolla.Consensus.Proposing.Packaging.Capping.Input
import           Dolla.Consensus.Proposing.Packaging.Capping.Sizable
import           Dolla.Consensus.Proposing.Packaging.Capping.Output


data State item
  = Initial
  | Adding { quantityAdded :: Byte , item :: item}
  | CuttingBecauseAboveSizeLimit {itemAboveLimit :: item, itemQuantity :: Byte}

capping
  :: ( S.MonadAsync m
     , Sizable item)
  => Byte
  -> S.SerialT m (Input item)
  -> S.SerialT m (Output item)
capping proposalSizeLimit input = capping' proposalSizeLimit input & S.concatMap S.fromList

capping'
  :: ( S.MonadAsync m
     , Sizable item)
  => Byte
  -> S.SerialT m (Input item)
  -> S.SerialT m [Output item]
capping' proposalSizeLimit input = input & S.postscan (cappingFold proposalSizeLimit)

cappingFold
  :: ( S.MonadAsync m
     , Sizable item)
  => Byte
  -> Fold m (Input item) [Output item]
cappingFold sizeLimit =
  Fold
    (\state event -> (state,event) & \case -- Transitions
      (Initial, AskForACut) -> return Initial
      (Initial, Transmit item) -> return Adding { quantityAdded = getMemorySize item, item}
      (Adding {..}, AskForACut ) -> return Initial
      (Adding {..}, Transmit newItem )
        -> do
           let itemSize = getMemorySize newItem
           if (quantityAdded + itemSize) > sizeLimit
           then return CuttingBecauseAboveSizeLimit { itemQuantity = itemSize, itemAboveLimit = newItem}
           else return Adding { quantityAdded = quantityAdded + getMemorySize newItem , item = newItem}
      (CuttingBecauseAboveSizeLimit {..}, AskForACut ) -> return Initial
      (CuttingBecauseAboveSizeLimit {..}, Transmit newItem )
         -> return Adding { quantityAdded = itemQuantity + getMemorySize newItem , item = newItem})
    (return Initial)
    (\case -- Events emission
        Initial -> return [Cut] -- just transmitting cuts
        Adding {item} -> return [Added item]
        CuttingBecauseAboveSizeLimit {itemAboveLimit} -> return [Cut,Added itemAboveLimit])



