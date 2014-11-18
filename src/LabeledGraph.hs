{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module LabeledGraph
  ( Graph
  , empty
  , addVertex
  , removeVertex
  , removeVertex'
  , addEdge
  , removeEdge
  , removeEdge'
  , getOutEdges
  , getOutEdges'
  , getVertex
  , vertices
  , indices
  , findIndex
  , toAdjacencyList'
  , toEdgeList
  , toEdgeList'
  , fromEdgeList
  , fromAdjacencyList ) where

import qualified Data.Map as M
import Data.Maybe
import Data.List

-- edge-labeled graph
-- TODO: move into labaled graph and/or use established implementation
data Graph k v l = Graph 
  { vertexMap :: M.Map k v
  , adjMap    :: M.Map k [(l,k)] } deriving (Eq,Ord,Show,Read)

empty :: Graph k v l
empty = Graph M.empty M.empty

addVertex :: (Vertex v k, Ord k) => v -> Graph k v l -> Graph k v l
addVertex v g = g { vertexMap = M.insert (index v) v (vertexMap g) }

removeVertex :: (Vertex v k, Ord k) => v -> Graph k v l -> Graph k v l
removeVertex = undefined

removeVertex' :: (Vertex v k, Ord k) => k -> Graph k v l -> Graph k v l
removeVertex' = undefined

addEdge :: (Ord k, Eq l, Vertex v k) => 
  v -> v -> l -> Graph k v l -> Graph k v l
addEdge v1 v2 l g = g 
  { vertexMap = M.insert k2 v2 . M.insert k1 v1 $ vertexMap g
  , adjMap    = M.insertWith (\x y -> nub $ x ++ y) k1 [(l,k2)] (adjMap g) }
  where k1 = index v1
        k2 = index v2

removeEdge :: (Ord k, Eq l, Vertex v k) => v -> v -> l -> Graph k v l -> Graph k v l
removeEdge = undefined

removeEdge' :: (Ord k, Eq l, Vertex v k) => k -> k -> l -> Graph k v l -> Graph k v l
removeEdge' = undefined

getVertex :: Ord k => k -> Graph k v l -> Maybe v
getVertex k g = M.lookup k (vertexMap g)

getOutEdges' :: (Vertex v k, Ord k) => k -> Graph k v l -> [(l,k)]
getOutEdges' k g = concat . maybeToList $ M.lookup k (adjMap g)

getOutEdges :: (Vertex v k, Ord k) => v -> Graph k v l -> [(l,v)]
getOutEdges v g = do
  (l,k) <- getOutEdges' (index v) g
  let Just v' = getVertex k g
  return (l,v')

getInEdges' :: (Vertex v k, Ord k) => k -> Graph k v l -> [(l,k)]
getInEdges' k g = undefined

vertices :: Graph k v l => [v]
vertices = M.elems . vertexMap

indices :: Graph k v l => [k]
indices = M.keys . vertexMap

findVertex :: (v -> Bool) -> Graph k v l -> [v]
findVertex p = filter p . vertices 

toAdjacencyList' :: Ord k => Graph k v l -> [(k,[(l,k)])]
toAdjacencyList' = M.toList . adjMap

toEdgeList' :: Ord k => Graph k v l -> [(k,l,k)]
toEdgeList' g = do
  (k1,xs) <- toAdjacencyList' g
  (l,k2) <- xs
  return (k1,l,k2)

toEdgeList :: Ord k => Graph k v l -> [(v,l,v)]
toEdgeList g = do
  (k1,l,k2) <- toEdgeList' g
  let Just v1 = getVertex k1 g
      Just v2 = getVertex k2 g
  return (v1,l,v2)

fromEdgeList :: (Vertex v k, Ord k, Eq l) => [(v,l,v)] -> Graph k v l
fromEdgeList = foldl' (\g (v1,l,v2) -> addEdge v1 v2 l g)  empty

fromAdjacencyList :: (Vertex v k, Ord k, Eq l) => [(v,[(l,v)])] -> Graph k v l
fromAdjacencyList xs = 
  fromEdgeList [(v1,l,v2) | (v1,lvs) <- xs, (l,v2) <- lvs ]

class Vertex v k where
  index :: v -> k

instance Vertex Int Int where
  index = id

instance Vertex String String where
  index = id

type IntGraph = Graph Int Int Int

