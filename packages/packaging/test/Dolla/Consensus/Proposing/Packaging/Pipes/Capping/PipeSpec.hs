{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dolla.Consensus.Proposing.Packaging.Pipes.Capping.PipeSpec
  (spec) where


import           GHC.Generics


import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold.Types as S

import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Input
import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.GenInput
import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Output

import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Pipe (capping)
import           Data.Function ((&))
import           Data.Coerce (coerce)
import           Dolla.Common.Memory.Byte hiding (getMemorySize)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Sizable
import           Data.Monoid

spec :: Spec
spec = do
  describe "About Capping item " $ do
      it "outputs a series of consecutives items having always a cumulated memory size <= limit given"
        $ property
        $ \inputs
            -> getGeneratedInputStream inputs
                & capping (10 * kb)
                & S.scan consecutivesMemorySizeAdded
                & S.mapM_ (`shouldSatisfy` (<= (10 * kb)))

getGeneratedInputStream 
  :: Monad m 
  => [InputUnderTests DummyRequest] 
  -> S.SerialT m (Input DummyRequest)
getGeneratedInputStream inputs 
  = S.fromList (coerce inputs :: [Input DummyRequest])

consecutivesMemorySizeAdded 
  :: Monad m 
  => S.Fold m (Output DummyRequest) Byte
consecutivesMemorySizeAdded
  = S.Fold
     (\sizeAccumulated event ->
        case event of
         Cut -> return (0*kb)
         Added item -> return (sizeAccumulated + getMemorySize item)
         )
     (return $ 0*kb)
     return

newtype DummyRequest = DummyRequest (Sum Int)
  deriving (Eq, Show, Generic)

instance Arbitrary DummyRequest where
  arbitrary = DummyRequest <$> arbitrary

instance Sizable DummyRequest where
  getMemorySize _ = 1 * kb

