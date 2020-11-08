{-# LANGUAGE RecordWildCards #-}
module Dolla.Consensus.Common.Zeus.Local.Node 
 ( getNode
 , getNodes
 , getNodesFolder
 , getNodeFolder
 , Node (..)
 , NodeId (..)
 , NodeIndex (..)
 )
 where

import           Data.Coerce (coerce)

import           Dolla.Common.NodeId
import           Dolla.Consensus.Common.Zeus.Local.NodeIndex

data Node = Node {nodeId :: NodeId ,nodeIndex::NodeIndex }

getNode :: Int -> Node
getNode index = Node {nodeId = getNodeId (coerce index), nodeIndex = coerce index}

getNodes :: Int -> [Node]
getNodes numberOfNodes = getNode <$> [0..numberOfNodes-1]

getNodeId :: NodeIndex -> NodeId
getNodeId index = NodeId $
  [ "achilles"
  , "ino"
  , "belus"
  , "heracles"
  , "calais"
  , "dardanus"
  , "epaphus"
  , "chiron"
  , "aeacus"
  , "agenor"
  , "amphion"
  , "cycnus"
  , "arcas"
  , "asclepius"
  , "aeneas"
  , "chrysaor"
  , "helen"
  , "memnon"
  ] !! coerce index

getNodesFolder :: FilePath ->  FilePath
getNodesFolder rootFolder = rootFolder ++ "nodes/"

getNodeFolder :: FilePath -> Node -> FilePath
getNodeFolder rootFolder Node {..} = getNodesFolder rootFolder ++ coerce nodeId ++ "/"
