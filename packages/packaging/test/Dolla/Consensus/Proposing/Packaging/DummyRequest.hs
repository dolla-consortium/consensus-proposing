{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Dolla.Consensus.Proposing.Packaging.DummyRequest
  (DummyRequest (..)) where


import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck

data DummyRequest
  = DummyRequest {fieldA :: Bool, fieldB :: Bool}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON DummyRequest

instance Arbitrary DummyRequest where
  arbitrary = DummyRequest <$> arbitrary <*> arbitrary

