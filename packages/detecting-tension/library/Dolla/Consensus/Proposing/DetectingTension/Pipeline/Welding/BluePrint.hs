{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dolla.Consensus.Proposing.DetectingTension.Pipeline.Welding.BluePrint
  () where

import           Dolla.Common.Pipeline.Weldable


import qualified Dolla.Consensus.Proposing.DetectingTension.Pipeline.IO.Input as DetectingTension

import qualified Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Input as DetectingTensedFlow
import qualified Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Output as DetectingTensedFlow

import qualified Dolla.Consensus.Proposing.DetectingTension.Pipeline.Sinking.Input as Sinking

instance Weldable DetectingTension.Input DetectingTensedFlow.Input where
  weld
    = \case
      DetectingTension.LocalProposalAccepted  -> DetectingTensedFlow.Released
      DetectingTension.LocalProposalStaged    -> DetectingTensedFlow.Staged
      DetectingTension.ConsensusReached       -> DetectingTensedFlow.Pulled

instance Weldable DetectingTensedFlow.Output Sinking.Input  where
  weld 
    = \case
      DetectingTensedFlow.Tensed -> Sinking.SinkLocalProposalFlowTensed
      
