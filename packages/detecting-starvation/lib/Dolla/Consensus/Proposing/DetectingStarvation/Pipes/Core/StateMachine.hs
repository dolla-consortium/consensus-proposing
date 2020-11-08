{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipes.Core.StateMachine
  ( isNewLocalProposalAsked
  , projection
  , State (..)
  ) where

import           Prelude hiding (log,writeFile)

import           Data.Monoid
import qualified Streamly.Internal.Data.Fold as SF
import           Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.IO.Input


type RemainingProposalToConsume = Integer

data State
  = State
    { remainingProposalToConsume :: Integer
    , isConsensusReached :: Any} deriving Eq

isNewLocalProposalAsked :: State -> Bool
isNewLocalProposalAsked = (== State 0 (Any True))

projection
  ::  (Monad m)
  => SF.Fold m Input State
projection
  = State <$> foldRemainingProposalToConsume
          <*> foldIsConsensusReached


foldIsConsensusReached :: (Monad m , Monoid Any) => SF.Fold m Input Any
foldIsConsensusReached
  = SF.foldMap
      (\case
        HandleConsensusReached  -> Any True
        _ -> Any False )

foldRemainingProposalToConsume :: (Monad m ) => SF.Fold m Input RemainingProposalToConsume
foldRemainingProposalToConsume
  = SF.lmapM
      (\inputItem -> do
       return $ case inputItem of
         HandleLocalProposalProduced ->  1
         HandleLocalProposalConsumed -> -1
         HandleConsensusReached    ->  0)
       SF.sum

