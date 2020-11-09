{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Receptioning.Service.OutputSpec (spec) where


import           Data.Aeson
import           GHC.Generics
import           Dolla.Adapter.Aeson.AesonVia 

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Text.InterpolatedString.Perl6 (qc)

import           Dolla.Consensus.Proposing.Receptioning.Service.Output

data DummyRequest
  = DummyRequest {fieldA :: Bool, fieldB :: Bool}
  deriving (Eq,Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON DummyRequest

spec :: Spec
spec = parallel $
  describe "Proposing Receptioning Output" $ do
      it "follow Proposing Staging Input json protocol " $ do
        encode (Receptioned (DummyRequest {fieldA = True, fieldB = False}) :: (Output DummyRequest))
          `shouldBe` [qc|\{"tag":"Receptioned","contents":\{"tag":"DummyRequest","fieldA":true,"fieldB":false}}|]


