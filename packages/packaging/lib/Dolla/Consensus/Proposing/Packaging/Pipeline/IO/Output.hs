{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output
  ( Output (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Common.Offset (Offset)
import           Dolla.Libraries.LogEngine.Appendable

newtype Output
  = LocalProposalPackaged {localOffset :: Offset}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via DefaultJSON Output

instance Appendable Output where
  getItemName LocalProposalPackaged {} = "LocalProposalPackaged"