module Dolla.Consensus.Proposing.Packaging.Capping.Sizable
  ( Sizable (..))
  where

import           Dolla.Common.Memory.Byte (Byte)

class Sizable a where
  getMemorySize :: a -> Byte

