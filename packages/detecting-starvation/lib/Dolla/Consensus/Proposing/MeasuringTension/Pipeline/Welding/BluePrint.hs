{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Welding.BluePrint
  () where

import           Dolla.Common.Pipeline.Weldable


import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipeline.IO.Input as MeasuringTension

import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Input as DetectingTensedFlow
import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipes.DetectingTensedFlow.Output as DetectingTensedFlow

import qualified Dolla.Consensus.Proposing.MeasuringTension.Pipeline.Sinking.Input as Sinking

instance Weldable MeasuringTension.Input DetectingTensedFlow.Input where
  weld
    = \case
      MeasuringTension.LocalProposalAccepted  -> DetectingTensedFlow.Released
      MeasuringTension.LocalProposalStaged    -> DetectingTensedFlow.Staged
      MeasuringTension.ConsensusReached       -> DetectingTensedFlow.Pulled

instance Weldable DetectingTensedFlow.Output Sinking.Input  where
  weld 
    = \case
      DetectingTensedFlow.Tensed -> Sinking.SinkLocalProposalFlowTensed
      
