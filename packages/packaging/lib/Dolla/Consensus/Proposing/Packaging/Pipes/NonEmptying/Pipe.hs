{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Dolla.Consensus.Proposing.Packaging.Pipes.NonEmptying.Pipe
  ( nonEmptying)
  where

import           Prelude hiding (log,writeFile)

import           Data.Function ((&))

import qualified Streamly.Prelude as S hiding (length,bracket)
import qualified Streamly as S
import           Streamly.Internal.Data.Fold.Types

nonEmptying
  :: ( S.MonadAsync m)
  => S.SerialT m (Maybe item)
  -> S.SerialT m (Maybe item)
nonEmptying input = nonEmptying' input & S.concatMap S.fromList

nonEmptying'
  :: ( S.MonadAsync m)
  => S.SerialT m (Maybe item)
  -> S.SerialT m [Maybe item]
nonEmptying' input = input & S.postscan nonEmptyingFold

nonEmptyingFold
  :: ( S.MonadAsync m)
  => Fold m (Maybe item) [Maybe item]
nonEmptyingFold  =
  Fold
    (\(_,b) event -> return (b,event))
    (return (Nothing,Nothing))
    (\case -- Events emission
        (Nothing,Nothing) -> return [] -- removing consecutives Nothing events
        (Just _ , Nothing) -> return [Nothing]
        (_,Just item) -> return [Just item]
        )



