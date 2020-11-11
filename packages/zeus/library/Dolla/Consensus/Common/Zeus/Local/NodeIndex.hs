{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Common.Zeus.Local.NodeIndex (NodeIndex (..)) where

newtype NodeIndex = NodeIndex { unNodeIndex :: Int }
 deriving (Eq,Show,Num) via Int
