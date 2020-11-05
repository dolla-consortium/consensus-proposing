{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.StateMachineSpec (spec) where

import           Data.Coerce (coerce)
import           Data.Monoid

import           Test.QuickCheck 
import           Test.QuickCheck.Instances ()
import           Test.Hspec


import qualified Streamly.Prelude as S
import qualified Streamly as S

import           Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.IO.GenInput

import           Dolla.Consensus.Proposing.Detecting.Starvation.Pipeline.StateMachine


spec :: Spec
spec = parallel $
  describe "State Machine used for detecting a local proposal starvation " $ do
    it "provides the number of remaining local proposal not yet consumed" 
      $ property 
      $ \inputs -> do
           let expectedRemainingProposalToConsume  = getExpectedRemainingProposalToConsume inputs
           State {remainingProposalToConsume} <- S.fold projection (getGeneratedInputStream inputs)
           remainingProposalToConsume `shouldBe` expectedRemainingProposalToConsume
    it "indicates when a local proposal is asked by the pipeline"
      $ property 
      $ \inputs -> do
           let expectedIsLocalProposalAsked  = getExpectedIsLocalProposalAsked inputs
           State{isLocalProposalAsked} <-  S.fold projection (getGeneratedInputStream inputs)
           isLocalProposalAsked `shouldBe` expectedIsLocalProposalAsked
 
getGeneratedInputStream
  :: Monad m
  => ConsistentInputs
  ->  S.SerialT m Input
getGeneratedInputStream consistentInputs =  S.fromList (coerce consistentInputs)

getExpectedRemainingProposalToConsume :: ConsistentInputs -> Integer
getExpectedRemainingProposalToConsume inputs = getSum $ delta  (coerce inputs)

delta :: [Input] -> Sum Integer
delta
  = foldMap
      (\case
         LocalProposalConsumed -> - 1
         LocalProposalProduced -> 1
         LocalProposalAsked -> 0)

getExpectedIsLocalProposalAsked :: ConsistentInputs -> Any
getExpectedIsLocalProposalAsked inputs = foldIsLocalProposalAsked (coerce inputs)

foldIsLocalProposalAsked :: [Input] -> Any
foldIsLocalProposalAsked
  = foldMap
      (\case
        LocalProposalAsked  -> Any True
        _ -> Any False )