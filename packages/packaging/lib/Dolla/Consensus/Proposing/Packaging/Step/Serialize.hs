{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Serialize (serialize) where

import           Prelude
import           Data.Function ((&))
import           Data.ByteString hiding (length,map,append)
import           Data.Word (Word8)
import           Data.Aeson.Types (ToJSON)

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S

import           Dolla.Common.UUID.Deterministic

serialize
  :: ( Monad m, ToJSON a)
  => S.SerialT m a
  -> S.SerialT m [Word8]
serialize input =  input & S.map (unpack . getEncodedItem)
