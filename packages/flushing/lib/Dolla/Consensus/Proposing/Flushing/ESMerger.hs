{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

module Dolla.Consensus.Proposing.Flushing.ESMerger (loadFlushingInputProjection) where

import           Prelude hiding (log)
import           Data.Coerce (coerce)
import qualified Dolla.Libraries.LogEngine.Instances.EventStore.Settings as EventStore
import           Dolla.Libraries.LogEngine.Instances.EventStore.Projection.Client
import           Text.InterpolatedString.Perl6 (qc)
import           Dolla.Common.NodeId
import           Control.Monad.Reader (ReaderT,ask, withReaderT)
import           Dolla.Common.Logging.Core
import           Dolla.Libraries.LogEngine.Instances.EventStore.Projection.Definition
import           Dolla.Consensus.Log.LogNameIndex


-- | User Defined Projection loaded into the event store for dispatching RB Broadcast Merged output and Result of Voting
--   events into the proper input stream for the maestro
--    * for more details about event store projection : https://eventstore.org/docs/projections/api/index.html
loadFlushingInputProjection :: ReaderT (NodeId, EventStore.Dependencies) IO ()
loadFlushingInputProjection = do
  (nodeId , EventStore.Dependencies {..}) <- ask
  let projectionName = coerce nodeId ++ "_flushing_input"
      maestroOutputMergedLogStreamName = getStreamNameFromIndex MaestroOutputMergedLog
      proposingPackagingOutputLogStreamName = getStreamNameFromIndex ProposingPackagingOutputLog
      body = [qc| options(\{
                         reorderEvents: false,
                         processingLag: 0
                     })
                     fromStreams ([ '{maestroOutputMergedLogStreamName}'
                                  , '{proposingPackagingOutputLogStreamName}'])
                     .when(\{
                         $any : function(s,e)\{
                           function getOutputStream() \{
                                return "flushing_input"
                           }
                           if (e.eventType == "ProposalAccepted" ) \{
                            linkTo ( getOutputStream(), e, \{})
                          }
                          if (e.eventType == "LocalProposalProduced" ) \{
                            linkTo ( getOutputStream(), e, \{})
                          } // if
                         } // $any
                     })
                     .outputState()|]

  log logger DEBUG "Loading Projection for Maestro Input  "
  -- TODO : Verified if the right projection version is loaded 
  result <- withReaderT snd $ createContinuousProjection
                              (ProjectionName projectionName)
                              (ProjectionBody body)
                              (ProjectionEnabled True)
                              (ProjectionEmitting True)
                              (ProjectionTrackEmittedStreams True)
  log logger DEBUG $ "Projection loaded : " ++ show result
  return ()

