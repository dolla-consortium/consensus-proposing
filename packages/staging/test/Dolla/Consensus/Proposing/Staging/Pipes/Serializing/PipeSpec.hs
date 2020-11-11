{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Dolla.Consensus.Proposing.Staging.Pipes.Serializing.PipeSpec
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

import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Input
import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Output
import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.GenInput
import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.Pipe (serializing)
import           Dolla.Consensus.Proposing.Staging.Pipes.Serializing.SerializedRequest
import           Dolla.Consensus.Proposing.Staging.DummyRequest

spec :: Spec
spec = parallel $
  describe "Proposing.Staging.Serializing" $ do
    it "Serialize requests when available"
      $ property
      $ \inputs ->
          aggregateInputToResult
              (getGeneratedInputStream inputs)
              serializing
            & S.mapM_
              (\case
                 (Nothing, Output result)
                   -> result `shouldBe` Nothing
                 (Just request@DummyRequest {}, Output (Just result) )
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