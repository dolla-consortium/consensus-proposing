{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Staging.Pipeline.IO.Input
  ( Input (..)) where

import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia 

-- Data Origins
-- The Staging input stream is the junction of 2 upstream pipelines :
-- - Receptioning : providing collected requests
-- - Detecting-Tension : notify when the local proposal flow is tensed. 

data Input request
  = Stage -- ^ ask to "Staging Pipeline" to force the stage of a new local proposal with all the requests currently collected
  | Package request -- ^ ask to to "Staging Pipeline" to package the request into a proposal according
                    -- some properties (see README.md)
  deriving (Eq,Show,Generic)
  deriving (FromJSON) via DefaultJSON (Input request)


