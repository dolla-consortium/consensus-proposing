{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dolla.Consensus.Proposing.DetectingTension.Pipes.DetectingTensedFlow.Output
  (Output (..)) where

data Output = Tensed deriving (Eq, Show)


