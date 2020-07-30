{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Dolla.Consensus.Proposer.Output
  (Output (..))
  where

import           Prelude hiding (id)
import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Libraries.LogEngine.Appendable

data Output = LocalProposalProduced
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON Output

instance Appendable Output where
  getItemName LocalProposalProduced {} = "LocalProposalProduced"
