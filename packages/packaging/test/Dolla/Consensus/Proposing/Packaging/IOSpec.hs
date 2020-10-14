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
module Dolla.Consensus.Proposing.Packaging.IOSpec (spec) where


import           Data.UUID
import           Data.Maybe   
import           Data.Aeson

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck
import           Test.Hspec
import           Text.InterpolatedString.Perl6 (qc)

import           Dolla.Consensus.Proposing.Packaging.Input

import           Dolla.Consensus.Proposing.Packaging.Output
import           Dolla.Consensus.Proposing.Packaging.GenOutput
import           Dolla.Consensus.Proposing.Packaging.DummyRequest

spec :: Spec
spec = parallel $
  describe "IO protocols" $ do
    describe "Input" $ do
      it "follow the json protocol" $ do
        encode (PipelineStarving {itemId = (fromJust . fromString) "bf428e1d-f221-55de-a77f-a61755a4d727"} :: (Input DummyRequest))
          `shouldBe` [qc|\{"tag":"PipelineStarving","itemId":"bf428e1d-f221-55de-a77f-a61755a4d727"}|]
        encode (RequestData (DummyRequest {fieldA = True, fieldB = False}) :: (Input DummyRequest))
          `shouldBe` [qc|\{"tag":"RequestData","contents":\{"tag":"DummyRequest","fieldA":true,"fieldB":false}}|]
    describe "Output" $ do
      it "follow Broadcasting Input json protocol "
        $ property $ \OutputUnderTests {unOutputUnderTests}
            -> encode unOutputUnderTests `shouldBe` [qc|\{"tag":"LocalProposalProduced","localOffset":{show $ localOffset unOutputUnderTests}}|]

