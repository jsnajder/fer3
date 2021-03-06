{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module Data.EdgeLabeledGraph
  ( Vertex (..)
  , Graph
  , empty
  , addVertex
  , removeVertex
  , removeVertex'
  , removeVertices
  , removeVertices'
  , addEdge
  , removeEdge
  , removeEdge'
  , outEdges
  , outEdges'
  , inEdges'
  , vertex
  , vertices
  , indices
  , findIndex
  , updateKeys
  , toAdjacencyList'
  , toEdgeList
  , toEdgeList'
  , fromEdgeList
  , fromAdjacencyList
  , filterEdges
  , modifyEdges
  , union 
  , unions
  , fromTree
  , toTree
  , combineEdges
  , findVertex
  , modifyVertex ) where

import Data.List hiding (union)
import qualified Data.Map as M
import Data.Maybe
import Data.Tree

-- edge-labeled graph

data Graph k v l = Graph 
  { vertexMap :: M.Map k v
  , adjMap    :: M.Map k [(l,k)] } deriving (Eq,Ord,Show,Read)

empty :: Graph k v l
empty = Graph M.empty M.empty

addVertex :: (Vertex v k, Ord k) => v -> Graph k v l -> Graph k v l
addVertex v g = g { vertexMap = M.insert (index v) v (vertexMap g) }

removeVertex :: (Vertex v k, Ord k) => v -> Graph k v l -> Graph k v l
removeVertex = removeVertex' . index

removeVertex' :: (Vertex v k, Ord k) => k -> Graph k v l -> Graph k v l
removeVertex' k g = g 
  { vertexMap = M.delete k $ vertexMap g
  , adjMap    = M.map (filter ((/=k) . snd)) . M.delete k $ adjMap g }

removeVertices' :: (Vertex v k, Ord k) => [k] -> Graph k v l -> Graph k v l
removeVertices' ks g = foldl' (flip removeVertex') g ks

removeVertices :: (Vertex v k, Ord k) => [v] -> Graph k v l -> Graph k v l
removeVertices vs g = foldl' (flip removeVertex) g vs

addEdge :: (Ord k, Eq l, Vertex v k) => 
  v -> v -> l -> Graph k v l -> Graph k v l
addEdge v1 v2 l g = g 
  { vertexMap = M.insert k2 v2 . M.insert k1 v1 $ vertexMap g
  , adjMap    = M.insertWith (\x y -> nub $ y ++ x) k1 [(l,k2)] (adjMap g) }
  where k1 = index v1
        k2 = index v2

removeEdge :: (Ord k, Eq l, Vertex v k) => v -> v -> l -> Graph k v l -> Graph k v l
removeEdge v1 v2 l = removeEdge' (index v1) (index v2) l

removeEdge' :: (Ord k, Eq l, Vertex v k) => k -> k -> l -> Graph k v l -> Graph k v l
removeEdge' k1 k2 l g = g
  { adjMap = M.adjust (delete (l,k2)) k1 (adjMap g) }

-- NB: this will also remove singleton vertices
-- TODO: fix this
filterEdges :: (Ord k, Eq l, Vertex v k) => 
  (v -> v -> l -> Bool) -> Graph k v l -> Graph k v l
filterEdges p = fromEdgeList . filter (\(v1,l,v2) -> p v1 v2 l) . toEdgeList

modifyEdges :: (Ord k, Eq l, Vertex v k) => 
  ((v,l,v) -> Maybe (v,l,v)) -> Graph k v l -> Graph k v l
modifyEdges f = fromEdgeList . mapMaybe f . toEdgeList

modifyVertex :: (Vertex v k, Ord k, Eq l) => 
  v -> (v -> v) -> Graph k v l -> Graph k v l
modifyVertex v f g = g { vertexMap = M.adjust f (index v) (vertexMap g) }

filterVertices = undefined

vertex :: Ord k => k -> Graph k v l -> Maybe v
vertex k = M.lookup k . vertexMap

outEdges' :: (Vertex v k, Ord k) => k -> Graph k v l -> [(l,k)]
outEdges' k g = concat . maybeToList $ M.lookup k (adjMap g)

outEdges :: (Vertex v k, Ord k) => v -> Graph k v l -> [(l,v)]
outEdges v g = do
  (l,k) <- outEdges' (index v) g
  let Just v' = vertex k g
  return (l,v')

outDegree' :: (Vertex v k, Ord k) => k -> Graph k v l -> Int
outDegree' k = length . outEdges' k

inEdges' :: (Vertex v k, Ord k) => k -> Graph k v l -> [(l,k)]
inEdges' k g = [ (l,k1) | (k1,l,k2) <- toEdgeList' g, k2==k ]

vertices :: Graph k v l -> [v]
vertices = M.elems . vertexMap

indices :: Graph k v l -> [k]
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
  let Just v1 = vertex k1 g
      Just v2 = vertex k2 g
  return (v1,l,v2)

fromEdgeList :: (Vertex v k, Ord k, Eq l) => [(v,l,v)] -> Graph k v l
fromEdgeList = foldl' (\g (v1,l,v2) -> addEdge v1 v2 l g)  empty

fromAdjacencyList :: (Vertex v k, Ord k, Eq l) => [(v,[(l,v)])] -> Graph k v l
fromAdjacencyList xs = 
  fromEdgeList [(v1,l,v2) | (v1,lvs) <- xs, (l,v2) <- lvs ]

updateKeys :: (Ord k, Eq l) => (k -> k) -> Graph k v l -> Graph k v l
updateKeys f g = Graph 
  { vertexMap = M.mapKeys f $ vertexMap g
  , adjMap    = M.mapKeys f (M.map (nub . map (\(l,k2) -> (l,f k2))) $ adjMap g) }

union :: (Vertex v k, Ord k, Eq l) => Graph k v l -> Graph k v l -> Graph k v l
union g1 g2 = foldl' (\g (v1,l,v2) -> addEdge v1 v2 l g) g1 (toEdgeList g2)

unions :: (Vertex v k, Ord k, Eq l) => [Graph k v l] -> Graph k v l
unions [] = empty
unions gs = foldl1 union gs

fromTree :: (Vertex v k, Ord k, Eq l) => (Int -> l) -> Tree v -> Graph k v l
fromTree f = fromEdgeList . map (\(v1,d,v2) -> (v1,f d,v2)) . treeEdges

treeEdges :: Tree v -> [(v,Int,v)]
treeEdges = edges 0
  where edges d (Node l ns) = map (\n -> (l,d,rootLabel n)) ns ++ 
                              concatMap (edges $ d+1) ns

-- constructs a tree rooted in v, traversing edges for which label evaluates to True
-- If the graph has dicycles, the tree will be infinite
toTree :: (Vertex v k, Ord k) => v -> (l -> Bool) -> Graph k v l -> Tree v
toTree v p g = Node v [toTree v p g | (l,v) <- outEdges v g, p l ]

combineEdges :: (Vertex v1 k, Vertex v2 k, Eq v2, Ord k, Eq l1, Eq l2) => 
  Graph k v1 l1 -> Graph k v2 l2 -> Graph k v1 (l1,l2)
combineEdges g1 g2 = fromEdgeList . concatMap f $ toEdgeList g1
  where f (v1,l1,v2) = [ (v1,(l1,l2),v2) | 
                         (l2,k2) <- outEdges' (index v1) g2, index v2 == k2 ]

class Vertex v k where
  index :: v -> k

instance Vertex Int Int where
  index = id

instance Vertex String String where
  index = id

type IntGraph = Graph Int Int Int

g = fromEdgeList [(1,12,2),(2,12,1),(2,23,3),(2,24,4)] :: IntGraph

-- add: to/from functional graph transformations
-- zapravo, ne, napravi novu verziju ovoga, tako da bude LIGHTWEIGHT omotač
-- oko FG-a. Trebaš pohranjivati node map (to već postoji, ali možda napravi
-- svoje) i trebaš pohranjivati key to node map



