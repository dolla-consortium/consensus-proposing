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
module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.InputSpec (spec) where

import           Data.Aeson

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Text.InterpolatedString.Perl6 (qc)

import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Input

import           Dolla.Consensus.Proposing.Packaging.DummyRequest

spec :: Spec
spec = parallel $
    describe "Input follow the integration protocol" $ do
      it "ForceProposalProduction is an interpretation of LocalProposalStarvationDetected from Detecting-Starvation Pipeline" $ do
        decode [qc|\{"tag":"LocalProposalStarvationDetected","itemId":"bf428e1d-f221-55de-a77f-a61755a4d727"}|]
          `shouldBe` (Just ForceProposalProduction  :: (Maybe (Input DummyRequest)))
      it "Package request is an interpretation of Receptioned request from Receptioning Pipeline" $ do
        decode [qc|\{"tag":"Receptioned","contents":\{"tag":"DummyRequest","fieldA":true,"fieldB":false}}|]
          `shouldBe` (Just $ Package (DummyRequest {fieldA = True, fieldB = False}) :: (Maybe (Input DummyRequest)))
    

