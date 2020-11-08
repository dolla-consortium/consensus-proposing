{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia 

-- Data Origins
-- The Packaging input stream is the junction of 2 upstream pipelines :
-- - Receptioning : providing collected requests
-- - Detecting-Starvation : transmitting Local-Proposal-Starvation Notifications

data Input request
  = Produce -- ^ ask to "Packaging Pipeline" to force the production of a new local proposal with all the requests currently collected
  | Package request -- ^ ask to to "Packaging Pipeline" to package the request into a proposal according
                    -- some properties (see README.md)
  deriving (Eq,Show,Generic)
  deriving (FromJSON) via DefaultJSON (Input request)


