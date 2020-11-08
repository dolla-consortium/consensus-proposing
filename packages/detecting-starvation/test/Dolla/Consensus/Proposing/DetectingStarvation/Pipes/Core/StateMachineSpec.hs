{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.StateMachineSpec (spec) where

import           Data.Coerce (coerce)
import           Data.Monoid

import           Test.QuickCheck 
import           Test.QuickCheck.Instances ()
import           Test.Hspec


import qualified Streamly.Prelude as S
import qualified Streamly as S

import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input
import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.GenInput

import           Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.StateMachine


spec :: Spec
spec = parallel $
  describe "State Machine used for detecting when a new local proposal is asked downstream the system" $ do
    it "provides the number of remaining local proposal not yet consumed" 
      $ property 
      $ \inputs -> do
           let expectedRemainingProposalToConsume  = getExpectedRemainingProposalToConsume inputs
           State {remainingProposalToConsume} <- S.fold projection (getGeneratedInputStream inputs)
           remainingProposalToConsume `shouldBe` expectedRemainingProposalToConsume
    it "indicates when a consensus is reached"
      $ property 
      $ \inputs -> do
           let expectedIsConsensusReached  = getExpectedIsConsensusReached inputs
           State{isConsensusReached} <-  S.fold projection (getGeneratedInputStream inputs)
           isConsensusReached `shouldBe` expectedIsConsensusReached
 
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
         HandleLocalProposalConsumed -> - 1
         HandleLocalProposalProduced -> 1
         HandleConsensusReached -> 0)

getExpectedIsConsensusReached :: ConsistentInputs -> Any
getExpectedIsConsensusReached inputs = foldIsConsensusReached (coerce inputs)

foldIsConsensusReached :: [Input] -> Any
foldIsConsensusReached
  = foldMap
      (\case
        HandleConsensusReached  -> Any True
        _ -> Any False )