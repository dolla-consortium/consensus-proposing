{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dolla.Consensus.Proposing.Packaging.NonEmptying.StreamProcessingSpec
  (spec) where

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S


import           Dolla.Consensus.Proposing.Packaging.NonEmptying.StreamProcessing (nonEmptying)
import           Data.Function ((&))

spec :: Spec
spec = do
  describe "NonEmptying" $ do
      it "The first event emitted is never Nothing"
        $ property $ \inputs
            -> do
               firstEvent <- S.fromList (inputs :: [Maybe ()])
                              & nonEmptying
                              & S.head
               firstEvent `shouldNotBe` Just Nothing
      it "Nothings are never emitted consecutively"
        $ property $ \inputs
            -> S.fromList (inputs :: [Maybe ()])
                 & nonEmptying
                 & S.rollingMap (,)
                 & S.mapM_ (`shouldNotBe` (Nothing,Nothing))





