{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
module Dolla.Consensus.Proposing.Starving.Detecting.OverMemoryStreamSpec (spec) where

import           Data.Coerce (coerce)
import           Data.Function ((&))
import           Data.Monoid
import           Data.Bifunctor

import           Control.Monad.Cont (liftIO)
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Monadic hiding (assert)
import           Test.QuickCheck.Instances ()
import           Test.Hspec

import qualified Streamly.Internal.Data.Fold as SF
import qualified Streamly.Prelude as S


import           Dolla.Consensus.Proposing.Starving.Detecting.Input
import           Dolla.Consensus.Proposing.Starving.Detecting.GenInput

import           Dolla.Consensus.Proposing.Starving.Detecting.OverMemoryStream

spec :: Spec
spec = parallel $
  describe "detecting a pipeline starving over a memory stream" $ do
    it "emits a Starving Pipeline output event when the predicate validate the projected state"
        $ property shouldEmitAnOutputWhenPredicateIsTrue


shouldEmitAnOutputWhenPredicateIsTrue :: ConsistentInputs -> Property
shouldEmitAnOutputWhenPredicateIsTrue consistentInputs
  = monadicIO $ do
      let indexedStreamOfTrueEvent
              = S.fromList (coerce consistentInputs)
              & S.postscan projectDeterministicallyAndRandomlyABool
              & S.postscan countTruesAndIndexIt
              & S.filter snd -- filter by true
          indexedStreamOfPipelineStarvingEvent
              = S.fromList (coerce consistentInputs)
              & detectingPipelineStarving
                  projectDeterministicallyAndRandomlyABool
                  withStarvingInvariantWhenTrue
              & S.indexed  & S.map (first (+1)) -- index starting by 1 and not 0
      liftIO
        $ S.drain
        $ S.zipAsyncWithM
        (\(pipelineStarvingEventOffset,_) (trueOffset,_) -> liftIO $ trueOffset `shouldBe` pipelineStarvingEventOffset)
        indexedStreamOfPipelineStarvingEvent
        indexedStreamOfTrueEvent


projectDeterministicallyAndRandomlyABool :: (Monad m) => SF.Fold m Input Bool
projectDeterministicallyAndRandomlyABool
  = SF.Fold
    (\(_,stdGen) _ -> do
       let (newInt,newGen) = next stdGen
       case newInt `mod` 2 of
        value | value == 0 -> return (True,newGen)
        _ -> return (False,newGen))
    (return (False,mkStdGen 0))
    (return . fst)


countTruesAndIndexIt :: (Monad m) => SF.Fold m Bool (Int,Bool)
countTruesAndIndexIt
  = (,)
      <$> (getSum <$> SF.foldMap
            (\case
              True  -> Sum 1
              False -> Sum 0))
      <*> SF.Fold
          (\_ a -> return a)
          (return False)
          return


withStarvingInvariantWhenTrue :: Bool -> Bool
withStarvingInvariantWhenTrue  = id