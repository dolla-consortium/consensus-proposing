{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Packaging.Serializing.StreamProcessingSpec
  (spec) where

import           Data.Aeson
import           Data.Function ((&))
import           Data.Coerce (coerce)
import           Data.ByteString.Lazy.Internal (unpackBytes)

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

import qualified Streamly.Prelude as S
import qualified Streamly as S

import           Dolla.Consensus.Proposing.Packaging.Serializing.Input
import           Dolla.Consensus.Proposing.Packaging.Serializing.Output
import           Dolla.Consensus.Proposing.Packaging.Serializing.GenInput
import           Dolla.Consensus.Proposing.Packaging.Serializing.StreamProcessing (serializing)
import           Dolla.Consensus.Proposing.Packaging.Serializing.SerializedRequest
import           Dolla.Consensus.Proposing.Packaging.DummyRequest

spec :: Spec
spec = parallel $
  describe "Proposing.Packaging.Serializing" $ do
    it "Serialize requests and Transmit downstream Pipeline Starving Notifications"
      $ property
      $ \inputs ->
          aggregateInputToResult
              (getGeneratedInputStream inputs)
              serializing
            & S.mapM_
              (\case
                 (TransmitPipelineStarvingDownStream, result)
                   -> result `shouldBe` PipelineStarving
                 (RequestToSerialize request@DummyRequest {}, RequestSerialized result)
                   -> result `shouldBe` (coerce.unpackBytes.encode) request
                 (a, b) -> expectationFailure $ "unexpected result = " ++ show (a,b) )

getGeneratedInputStream
  :: Monad m
  => [InputUnderTests DummyRequest]
  -> S.SerialT m (Input DummyRequest)
getGeneratedInputStream inputs
  = S.fromList (coerce inputs :: [Input DummyRequest])

aggregateInputToResult
  :: (S.MonadAsync m)
  => S.SerialT m a
  -> (S.SerialT m a -> S.SerialT m b)
  -> S.SerialT m (a,b)
aggregateInputToResult inputStream stream
  = do
    i <- inputStream
    result <- S.yield i & stream
    return (i,result)