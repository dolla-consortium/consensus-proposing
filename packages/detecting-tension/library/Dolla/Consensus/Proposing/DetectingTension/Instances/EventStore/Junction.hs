{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}

module Dolla.Consensus.Proposing.DetectingTension.Instances.EventStore.Junction
  (loadJunctionInEventStore) where

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

-- | A Junction (Merger) is
   --a set of persisted input streams
   --a nondeterministic logic for merging these input streams
   --a persisted output stream (input of a pipeline)

-- | Junction for Starving Detection Pipeline : Logic to generate the input stream of the pipeline.
--   We are using the "User Defined Projections" feature from the EventStore to implement this junction :
--    - javaScript snippets
--    - loaded in the event store microservice directly  
--    - more details : https://eventstore.org/docs/projections/api/index.html
loadJunctionInEventStore :: ReaderT (NodeId, EventStore.Dependencies) IO ()
loadJunctionInEventStore = do
  (nodeId , EventStore.Dependencies {..}) <- ask
  let projectionName = coerce nodeId ++ "_proposing_starving_detection_input"
      maestroOutputMergedLogStreamName = getStreamNameFromIndex MaestroOutputMergedLog
      proposingPackagingOutputLogStreamName = getStreamNameFromIndex ProposingPackagingOutputLog
      proposingStarvingDetectionInputLogStreamName = getStreamNameFromIndex ProposingStarvingDetectionInputLog
      body = [qc| options(\{
                         reorderEvents: false,
                         processingLag: 0
                     })
                     fromStreams ([ '{maestroOutputMergedLogStreamName}'
                                  , '{proposingPackagingOutputLogStreamName}'])
                     .when(\{
                          $any : function(s,e)\{
                           function getOutputStream() \{
                                return '{proposingStarvingDetectionInputLogStreamName}'
                          }
                          if (e.eventType == "ProposalAccepted" ) \{
                            var messageJson = JSON.parse(e.bodyRaw)
                            if (messageJson.byProposer.proposerId == {nodeId}) \{
                                emit ( getOutputStream ()
                                   , "LocalProposalAccepted"
                                   , "LocalProposalAccepted"
                                   , \{});
                            }
                          }
                          if (e.eventType == "ConsensusReached" ) \{
                            emit ( getOutputStream ()
                               , "ConsensusReached"
                               , "ConsensusReached"
                               , \{});
                          }
                          if (e.eventType == "LocalProposalStaged" ) \{
                            emit ( getOutputStream ()
                              , "LocalProposalStaged"
                              , "LocalProposalStaged"
                              , \{});
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

