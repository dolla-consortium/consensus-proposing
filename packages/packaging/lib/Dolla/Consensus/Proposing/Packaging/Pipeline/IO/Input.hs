{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input
  ( Input (..)) where

-- Data Origins
-- The Packaging input stream is the junction of 2 upstream pipelines :
-- - Receptioning : providing collected requests
-- - Detecting-Starvation : transmitting Local-Proposal-Starvation Notifications

import           Data.Aeson
import qualified Data.Text as Text


data Input request
  = ForceProposalProduction -- ^ ask to "Packaging Pipeline" to flush all the requests currently collected
  | Package request -- ^ ask to to "Packaging Pipeline" to package the request into a proposal according
                    -- some properties (see README.md)
  deriving (Eq,Show)


-- | Integration with upstream junction
-- We are reinterpreting events from upstream pipelines into commands for the new pipeline.
-- (See tests for a more details)
instance FromJSON request => FromJSON (Input request)  where
    parseJSON (Object jsonObject) = do
      tagMaybe <- jsonObject .: "tag"
      case Text.unpack <$> tagMaybe  of
        Just "LocalProposalStarvationDetected" -> return ForceProposalProduction
        Just "Receptioned" -> Package <$> jsonObject .: "contents"
        anythingElse -> error $ "tag " ++ show anythingElse  ++ "unknown, please verify integration with junction"
    parseJSON _ =  error $ "Json format not expected"

