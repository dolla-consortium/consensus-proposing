{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Dolla.Consensus.Proposing.Zeus.CLI (startInteractiveZeus) where

import           Prelude hiding (FilePath,writeFile)
import           System.Console.Byline hiding (askUntil,askWithMenuRepeatedly)

import           Turtle hiding (text)
import           Dolla.Adapter.ByLine.Wrapper
import qualified Dolla.Consensus.Proposing.Zeus.Local.CLI as Local.Proposer


data ZeusAction
  = StartProposerPipeline Text
  | StopProposerPipeline  Text
  | Quit Text

startInteractiveZeus :: MonadIO m => m ()
startInteractiveZeus
  = do
    echo "################################################"
    echo "|| Welcome on Zeus for the Proposing Pipeline ||"
    echo "################################################"
    void <$> liftIO $ runByline displayZeusMenu
    echo "################################################"
    echo "||              End of Zeus                   ||"
    echo "################################################"

displayZeusMenu :: MonadIO m =>  Byline m ()
displayZeusMenu
   = askWithMenuRepeatedly
       zeusMenu
       "> please choose an action (provide the index) : "
       "> please enter a valid index..."
       >>= \case
           StartProposerPipeline _ -> Local.Proposer.delete >> Local.Proposer.start >> displayZeusMenu
           StopProposerPipeline _  -> Local.Proposer.stop >> displayZeusMenu
           Quit _ -> return ()


zeusMenu :: Menu ZeusAction
zeusMenu
  = renderPrefixAndSuffixForDynamicMenu
    $ menu
      [ StartProposerPipeline "Run/Re-Run the proposer pipeline locally"
      , StopProposerPipeline  "Stop the proposer pipeline locally"
      , Quit                  "Quit Zeus"]
      (\case
        StartProposerPipeline description -> fg green <> text description
        StopProposerPipeline  description -> fg green <> text description
        Quit                  description -> fg green <> text description)