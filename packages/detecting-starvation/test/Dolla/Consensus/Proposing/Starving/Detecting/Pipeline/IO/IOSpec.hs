{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}

module Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.IOSpec (spec) where


import           Data.Aeson
import           Test.QuickCheck

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Text.InterpolatedString.Perl6 (qc)

import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Input

import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.Output
import           Dolla.Consensus.Proposing.Starving.Detecting.Pipeline.IO.GenOutput

spec :: Spec
spec = parallel $
  describe "Proposing.Starving-Detection IOs" $ do
    describe "Input" $ do
      it "follow the json protocol" $ do
        decode [qc| "LocalProposalAsked"    |] `shouldBe` Just LocalProposalAsked
        decode [qc| "LocalProposalConsumed" |] `shouldBe` Just LocalProposalConsumed
        decode [qc| "LocalProposalProduced" |] `shouldBe` Just LocalProposalProduced
    describe "Output" $ do
      it "follow Proposing Packaging Input Json Protocol"
        $ property $ \OutputUnderTests {unOutputUnderTests}
            -> do
               let encodedItemId = encode $ itemId unOutputUnderTests
               encode unOutputUnderTests `shouldBe` [qc|\{"tag":"LocalProposalStarvationDetected","itemId":{encodedItemId}}|]


