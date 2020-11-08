{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Staging.Pipes.Capping.Pipe
  ( capping)
  where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))


import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Streamly.Internal.Data.Fold.Types

import           Dolla.Common.Memory.Byte (Byte)

import           Dolla.Consensus.Proposing.Staging.Pipes.Capping.Input
import           Dolla.Consensus.Proposing.Staging.Pipes.Capping.Sizable
import           Dolla.Consensus.Proposing.Staging.Pipes.Capping.Output


data State item
  = Initial
  | Adding { quantityAdded :: Byte , item :: item}
  | AboveSizeLimit {itemAboveLimit :: item, itemQuantity :: Byte}

capping
  :: ( S.MonadAsync m
     , Sizable item)
  => Byte -- ^ proposalSizeLimit
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
      (Initial, Add item) -> return Adding { quantityAdded = getMemorySize item, item}
      (Adding {..}, AskForACut ) -> return Initial
      (Adding {..}, Add newItem )
        -> do
           let itemSize = getMemorySize newItem
           if (quantityAdded + itemSize) > sizeLimit
           then return AboveSizeLimit { itemQuantity = itemSize, itemAboveLimit = newItem}
           else return Adding { quantityAdded = quantityAdded + getMemorySize newItem , item = newItem}
      (AboveSizeLimit {..}, AskForACut ) -> return Initial
      (AboveSizeLimit {..}, Add newItem )
         -> return Adding { quantityAdded = itemQuantity + getMemorySize newItem , item = newItem})
    (return Initial)
    (\case -- Events emission
        Initial -> return [Cut] -- just transmitting cuts
        Adding {item} -> return [Added item]
        AboveSizeLimit {itemAboveLimit} -> return [Cut,Added itemAboveLimit])



