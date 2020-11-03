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
module Dolla.Consensus.Proposing.Packaging.Pipeline.IO.OutputSpec (spec) where

import           Data.Aeson

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck
import           Test.Hspec
import           Text.InterpolatedString.Perl6 (qc)

import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.Output
import           Dolla.Consensus.Proposing.Packaging.Pipeline.IO.GenOutput


spec :: Spec
spec = parallel $
    describe "Output" $ do
      it "follow Broadcasting Input json protocol "
        $ property $ \OutputUnderTests {unOutputUnderTests}
            -> encode unOutputUnderTests 
                `shouldBe` [qc|\{"tag":"LocalProposalProduced","localOffset":{show $ localOffset unOutputUnderTests}}|]

