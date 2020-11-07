{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.DetectingStarvation.Pipeline.StateMachine
  ( starvingPredicate
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
    , isLocalProposalAsked :: Any} deriving Eq

starvingPredicate :: State -> Bool
starvingPredicate = (== State 0 (Any True))

projection
  ::  (Monad m)
  => SF.Fold m Input State
projection
  = State <$> foldRemainingProposalToConsume
          <*> foldIsLocalProposalAsked


foldIsLocalProposalAsked :: (Monad m , Monoid Any) => SF.Fold m Input Any
foldIsLocalProposalAsked
  = SF.foldMap
      (\case
        LocalProposalAsked  -> Any True
        _ -> Any False )

foldRemainingProposalToConsume :: (Monad m ) => SF.Fold m Input RemainingProposalToConsume
foldRemainingProposalToConsume
  = SF.lmapM
      (\inputItem -> do
       return $ case inputItem of
         LocalProposalProduced ->  1
         LocalProposalConsumed -> -1
         LocalProposalAsked    ->  0)
       SF.sum
