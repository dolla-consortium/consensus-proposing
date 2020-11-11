{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
module Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.StateMachineSpec (spec) where

import           Data.Coerce (coerce)
import           Data.Monoid

import           Test.QuickCheck 
import           Test.QuickCheck.Instances ()
import           Test.Hspec


import qualified Streamly.Prelude as S
import qualified Streamly as S

import           Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Input
import           Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.GenInput

import           Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.StateMachine


spec :: Spec
spec = parallel $
  describe "State Machine used for detecting when a flow is tensed" $ do
    it "provides the number of items in transit withing the flow" 
      $ property 
      $ \inputs -> do
           let expectedItemsInTransit  = getExpectedItemsInTransit inputs
           State {itemsInTransit} <- S.fold projection (getGeneratedInputStream inputs)
           itemsInTransit `shouldBe` expectedItemsInTransit
    it "indicates when an item is pulled"
      $ property 
      $ \inputs -> do
           let expectedIsPulled  = getExpectedIsPulled inputs
           State{isPulled} <-  S.fold projection (getGeneratedInputStream inputs)
           isPulled `shouldBe` expectedIsPulled
 
getGeneratedInputStream
  :: Monad m
  => ConsistentInputs
  ->  S.SerialT m Input
getGeneratedInputStream consistentInputs =  S.fromList (coerce consistentInputs)

getExpectedItemsInTransit :: ConsistentInputs -> Integer
getExpectedItemsInTransit inputs 
  = getSum $ foldItemsInTransit  (coerce inputs)
  where
    foldItemsInTransit :: [Input] -> Sum Integer
    foldItemsInTransit
      = foldMap
          (\case
             Released -> - 1
             Staged -> 1
             Pulled -> 0)

getExpectedIsPulled :: ConsistentInputs -> Any
getExpectedIsPulled inputs 
  = foldIsPulled (coerce inputs)
  where 
  foldIsPulled :: [Input] -> Any
  foldIsPulled
    = foldMap
        (\case
          Pulled  -> Any True
          _ -> Any False )