{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.StateMachine
  ( isFlowTensed
  , projection
  , State (..)
  ) where

import           Prelude hiding (log,writeFile)

import           Data.Monoid
import qualified Streamly.Internal.Data.Fold as SF
import           Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Input


type RemainingProposalToConsume = Integer

data State
  = State
    { itemsInTransit :: Integer
    , isPulled :: Any} deriving Eq

isFlowTensed :: State -> Bool
isFlowTensed = (== State 0 (Any True))

projection
  ::  (Monad m)
  => SF.Fold m Input State
projection
  = State <$> foldItemsInTransit
          <*> foldIsPulled


foldIsPulled :: (Monad m , Monoid Any) => SF.Fold m Input Any
foldIsPulled
  = SF.foldMap
      (\case
        Pulled  -> Any True
        _ -> Any False )

foldItemsInTransit :: (Monad m ) => SF.Fold m Input RemainingProposalToConsume
foldItemsInTransit
  = SF.lmapM
      (\inputItem -> do
       return $ case inputItem of
         Staged ->  1
         Released -> -1
         Pulled    ->  0)
       SF.sum

