{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposing.Starving.Detecting.StateSpec (spec) where

import           Data.Coerce (coerce)
import           Data.Monoid
import           Control.Monad.Cont (liftIO)

import           Test.QuickCheck 
import           Test.QuickCheck.Monadic hiding (assert)
import           Test.QuickCheck.Instances ()
import           Test.Hspec


import qualified Streamly.Prelude as S


import           Dolla.Consensus.Proposing.Starving.Detecting.Input
import           Dolla.Consensus.Proposing.Starving.Detecting.GenInput

import           Dolla.Consensus.Proposing.Starving.Detecting.State


spec :: Spec
spec = parallel $
  describe "State used for detecting a starving pipeline" $ do
    it "provides the number of remaining local proposal not yet consumed" $ property shouldProjectNBRemainingProposalToConsume
    it "indicates when a local proposal is asked by the pipeline"         $ property shouldProjectIsLocalProposalAsked


shouldProjectNBRemainingProposalToConsume :: ConsistentInputs -> Property
shouldProjectNBRemainingProposalToConsume consistentInputs
  = monadicIO $ do
      let expectedRemainingProposalToConsume  = getSum $ delta  (coerce consistentInputs)
      actualRemainingProposalToConsume <- remainingProposalToConsume <$> S.fold projection (S.fromList (coerce consistentInputs))
      liftIO $ actualRemainingProposalToConsume `shouldBe` expectedRemainingProposalToConsume

shouldProjectIsLocalProposalAsked :: ConsistentInputs -> Property
shouldProjectIsLocalProposalAsked consistentInputs
  = monadicIO $ do
      let expectedIsLocalProposalAsked  = foldIsLocalProposalAsked  (coerce consistentInputs)
      actualIsLocalProposalAsked <- isLocalProposalAsked <$> S.fold projection (S.fromList (coerce consistentInputs))
      liftIO $ actualIsLocalProposalAsked `shouldBe` expectedIsLocalProposalAsked


delta :: [Input] -> Sum Integer
delta
  = foldMap
      (\case
         LocalProposalConsumed -> - 1
         LocalProposalProduced -> 1
         LocalProposalAsked -> 0)

foldIsLocalProposalAsked :: [Input] -> Any
foldIsLocalProposalAsked
  = foldMap
      (\case
        LocalProposalAsked  -> Any True
        _ -> Any False )