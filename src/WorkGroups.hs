
module WorkGroups where

import Catalogue
import Control.Applicative
import Control.Monad hiding (ap)
import qualified Data.Counts as C
import Data.Function
import Data.List
import qualified Data.EdgeLabeledGraph as G
import Data.Graph.Inductive 
import qualified Data.Map as M
import Data.Maybe
import Data.List.Partition (eqClasses)
import SimLists
import qualified SplitGraph as SG
import Text.Printf

type OverlapGraph = Gr ItemId ()

{-
mkOverlapGraph :: SimGraphPaired -> OverlapGraph
mkOverlapGraph g = undir $ mkGraph (zip [0..] nodes) edges
  where nodes = filter isUnit . nub . map lift $ G.vertices g
        edges = do
                  (v1,(l1,l2),v2) <- G.toEdgeList g
                  let (v1',v2') = (lift v1, lift v2)
                  guard $ isUnit v1' && isUnit v2'
                  guard $ l1 `elem` [O,E] || l2 `elem` [O,E]
                  let Just i1 = M.lookup v1' ix
                  let Just i2 = M.lookup v2' ix
                  return (i1,i2,())
        ix = M.fromList $ zip nodes [0..]
        isUnit x = (isJust . unitId $ x) && (topicId x == Nothing)
        lift x = x { topicId = Nothing } 
-}
-- without topic lifting
mkOverlapGraph :: SimGraphPaired -> OverlapGraph
mkOverlapGraph g = undir $ mkGraph (zip [0..] nodes) edges
  where nodes = filter isUnit $ G.vertices g
        edges = do
                  (v1,(l1,l2),v2) <- G.toEdgeList g
                  guard $ isUnit v1 && isUnit v2
                  guard $ l1 `elem` [O,E] || l2 `elem` [O,E]
                  let Just i1 = M.lookup v1 ix
                  let Just i2 = M.lookup v2 ix
                  return (i1,i2,())
        ix = M.fromList $ zip nodes [0..]
        isUnit x = (isJust . unitId $ x) && (topicId x == Nothing)

-- TODO: lift edges from topics to units

g = mkGraph (zip [1..5] "12345") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(3,4,"34"),(3,5,"35"),(4,5,"45")] :: Gr Char String

g2 = mkGraph (zip [1..6] "123456") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(6,4,"34"),(6,5,"35"),(4,5,"45")] :: Gr Char String

g3 = mkGraph (zip [1..6] "123456") 
            [(1,2,"12"),(2,3,"23"),(1,3,"13"),(3,4,"34"),(3,5,"35"),(4,5,"45"),(4,6,"46")] :: Gr Char String

{-
sccGraphs :: Graph gr => gr a b => [gr a b]
sccGraphs g = [ delNodes ys g | xs <- scc g, let ys = ns \\ xs ]
  where ns = nodes g
-}

loadOverlapGraph = do
  Right c <- loadCatalogue catFile
  fs <- csvFiles simDir
  g  <- filterSpurious <$> loadSimLists fs
  let og = mkOverlapGraph . pairGraph $ g
  return $ og

overlapClasses1 :: OverlapGraph -> [[ItemId]]
overlapClasses1 = map (map snd . labNodes) . filter ((>1) . noNodes) . SG.gComponents

overlapClasses2 :: OverlapGraph -> [[ItemId]]
overlapClasses2 = 
  map (map snd . labNodes) .  
  concatMap (SG.splitToSize 7) . filter ((>1) . noNodes) . SG.gComponents

type OverlapComponent = (Int, [(ItemId, [ItemId], [Int])])

overlapClasses2' :: OverlapGraph -> [OverlapComponent]
overlapClasses2' og = 
  [ (i, map (mkUnit i) $ nodes oc) | (i,oc) <- ocs ]
  where ocs  = zip [1..] . concatMap (SG.splitToSize 7) . 
               filter ((>1) . noNodes) $ SG.gComponents og
        gi x = map fst $ filter ((x `gelem`) . snd) ocs
        mkUnit i x = (fromJust $ lab og x, 
                      mapMaybe (lab og) . nub $ neighbors og x, 
                      filter (/=i) $ gi x)

analyseOverlaps cat og = putStr . unlines $
  [ printf "%d non-singleton components" (length oc) 
  , printf "%d nodes in non-singletons" n 
  , printf "components size histogram: %s" (show $ C.counts xs) 
  , printf "size-7-splitted components: %d" (length oc')
  , printf "components size histogram: %s" (show $ C.counts xs')
  , printf "longest shortest paths histogram: %s" (show $ C.counts pl)
  , printf "articulation points splitted: %d" a
  , printf "node multiplicity histogram: %s" (show $ C.counts zs)
  , printf "number of groups: %d" (length gs)
  , printf "group sizes (by components) histogram: %s" (show $ C.counts gcs)
  , printf "group sizes (by editors) histogram: %s" (show $ C.counts ges) ]
  where oc  = filter ((>1) . noNodes) $ SG.gComponents og
        n   = sum $ map noNodes oc
        xs  = C.fromList $ map noNodes oc
        oc' = concatMap (SG.splitToSize 7) oc
        n'  = sum $ map noNodes oc'
        xs' = C.fromList $ map noNodes oc'
        pl  = C.fromList $ map longestShortestPath oc'
        a   = length . nub $ concatMap nodes oc' \\ concatMap nodes oc
        zs  = C.fromList . map snd . C.counts . C.fromList $ concatMap nodes oc'
        gs  = mkGroups cat $ map (map snd . labNodes) oc'
        ges = C.fromList $ map (length . snd) gs
        gcs = C.fromList $ map (length . fst) gs

longestShortestPath :: Gr a b -> Int
longestShortestPath g = maximum $ map (longest g) (nodes g)
  where longest g x = maximum . map (\(LP p) -> length p) . spTree x $ emap (const 1) g
 
mkGroups :: Catalogue -> [[ItemId]] -> [([[Item]], [ItemEditor])]
mkGroups cat oc = 
  [ (map fst xs, e) | xs@((x,e):_) <- eqClasses ((==) `on` snd) xss' ]
  where xss = map (map (fromJust . getItem cat)) oc
        xss' = map (\x -> (x, groupEditors x)) xss

groupEditors :: [Item] -> [ItemEditor]
groupEditors = nub . sort . concatMap itemEditors

-- TODO: indicate shared KUs

foo = do
  og <- loadOverlapGraph
  Right c <- loadCatalogue catFile
  let oc = overlapClasses2 og
  return $ mkGroups c oc
 
