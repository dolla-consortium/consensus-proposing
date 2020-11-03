module Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.SerializedRequest
  ( SerializedRequest (..))
  where

import           Data.Word (Word8)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Sizable

newtype SerializedRequest = SerializedRequest [Word8] deriving (Eq,Show)

instance Sizable SerializedRequest where
  getMemorySize (SerializedRequest word8s) = (fromIntegral . length) word8s



