{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.PipeSpec
  (spec) where

import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Function ((&))
import           Data.Maybe
import           Data.Word (Word8)
import           Data.Aeson
import           Data.List.Utils (replace)
import           Data.UUID (UUID)

import           Control.Monad.Cont (liftIO, MonadIO)
import           System.Directory (createDirectoryIfMissing,getCurrentDirectory)
import           System.Random (randomIO)
import           GHC.Generics

import           Turtle (stdout)

import           Test.QuickCheck.Instances ()
import           Test.Hspec
import           Test.QuickCheck

import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as SF

import           Dolla.Adapter.Aeson.AesonVia
import           Dolla.Adapter.Turtle.Wrapper (echoCommandAndInShell)

import           Dolla.Common.Data.IsString
import           Dolla.Common.Offset
import           Dolla.Common.Memory.Byte hiding (getMemorySize)

import           Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Input
import           Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Output

import           Dolla.Consensus.Proposing.Packaging.Pipes.Persisting.Pipe (persisting)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Pipe (serializing)
import           Dolla.Consensus.Proposing.Packaging.Pipes.NonEmptying.Pipe (nonEmptying)

import           Dolla.Consensus.Proposing.Packaging.Pipes.Capping.Sizable
import           Dolla.Consensus.Proposal.Persistence (ProposalRootFolder, getLocalProposalFolder)
import           Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.SerializedRequest

import qualified Dolla.Consensus.Proposing.Packaging.Pipes.Serializing.Output as Serializing

spec :: Spec
spec = do
  let testFolder = "/output-test/"
  parallel $ describe "Persisting" 
      $ beforeAll_ (setUp testFolder) 
      $ afterAll_  (tearDown testFolder)  
      $ do
        it "should save consecutives `Persist item` into a file `x.proposal` where x is the local offset of the proposal "
        $ property $ \inputs
            -> do
               testId <- liftIO randomIO :: IO UUID
               let proposalRelativePathFolder = testFolder ++ show testId ++ "/"
               proposalRootFolder <- liftIO $ (++ proposalRelativePathFolder) <$> getCurrentDirectory
               (getGeneratedEncodedInputStream inputs & persisting proposalRootFolder)
                  & S.mapM_ (\LocalProposalPersisted {proposalId}
                       -> do
                          expectedContent <- getExpectedProposalContent inputs proposalId
                          actualContent <- readProposalPersisted proposalRootFolder proposalId
                          actualContent `shouldBe` expectedContent )



getExpectedProposalContent :: S.MonadAsync m => [Maybe DummyRequest] -> Offset -> m [DummyRequest]
getExpectedProposalContent inputs offset
  = do
    proposals <- S.splitOn (== CommitProposal) SF.toList (getGeneratedInputStream inputs)
                      & S.map (\expectedContent -> [x | Persist x <- expectedContent])
                      & S.filter (/= mempty)
                      & S.toList
    return $ proposals !! offsetToIntegral offset


setUp :: String -> IO ()
setUp proposalRelativePathFolder
  = do
    proposalRootFolder <- liftIO $ (++ proposalRelativePathFolder) <$> getCurrentDirectory
    createDirectoryIfMissing True proposalRootFolder
    stdout $ echoCommandAndInShell $ "rm -rf "<++> proposalRootFolder

tearDown :: String -> IO ()
tearDown proposalRelativePathFolder
  = do
    proposalRootFolder <- liftIO $ (++ proposalRelativePathFolder) <$> getCurrentDirectory
    stdout $ echoCommandAndInShell $ "rm -rf "<++> proposalRootFolder

readProposalPersisted
  :: (MonadIO m)
  => ProposalRootFolder
  -> Offset
  -> m [DummyRequest]
readProposalPersisted proposalRootFolder localProposalOffset
  = do
    let filepath = getLocalProposalFolder proposalRootFolder ++  show localProposalOffset ++ ".proposal"
    fileContentAsString <- liftIO $ readFile filepath
    -- Requests are not saved in a JSON conventional way
    -- {..}{..}{..} instead of [{..},{..},{..}]
    return $ fromJust . decode . pack $ "[" ++ replace "}{" "},{" fileContentAsString ++ "]" 

getGeneratedEncodedInputStream
  :: S.MonadAsync m
  => [Maybe DummyRequest]
  -> S.SerialT m (Input [Word8])
getGeneratedEncodedInputStream inputs
  = getGeneratedInputStream inputs
      & S.map
      (\case
        CommitProposal -> Nothing
        Persist a -> Just a )
      & serializing
        & S.map
              (\case
                Serializing.Output Nothing -> CommitProposal
                Serializing.Output (Just (SerializedRequest word8s)) -> Persist word8s)

getGeneratedInputStream 
  :: S.MonadAsync m
  => [Maybe DummyRequest]
  -> S.SerialT m (Input DummyRequest)
getGeneratedInputStream inputs 
  = S.fromList inputs
      & nonEmptying
      & S.map
        (\case
          Nothing -> CommitProposal
          Just a -> Persist a )


newtype DummyRequest = DummyRequest Int
  deriving (Eq, Show, Generic)
  deriving (ToJSON,FromJSON) via DefaultJSON DummyRequest

instance Arbitrary DummyRequest where
  arbitrary = DummyRequest <$> arbitrary

instance Sizable DummyRequest where
  getMemorySize _ = 1 * kb

