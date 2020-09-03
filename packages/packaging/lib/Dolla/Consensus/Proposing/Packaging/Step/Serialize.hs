{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Step.Serialize (serialize) where

import           Prelude
import           Data.ByteString hiding (length,map,append)
import           Data.Word (Word8)
import           Data.Aeson.Types (ToJSON)

import qualified Streamly as S

import           Dolla.Common.UUID.Deterministic
import           Dolla.Consensus.Proposing.Packaging.Input

serialize
  :: ( Monad m, ToJSON a)
  => S.SerialT m (Input a)
  -> S.SerialT m (Input [Word8])
serialize xs =  xs >>= (\input -> return $ unpack . getEncodedItem <$> input) 