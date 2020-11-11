module Dolla.Consensus.Proposing.Staging.Pipes.Capping.Sizable
  ( Sizable (..))
  where

import           Dolla.Common.Memory.Byte (Byte)

class Sizable a where
  getMemorySize :: a -> Byte

