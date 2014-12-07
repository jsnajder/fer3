
module WorkGroups where

import Catalogue
import Control.Applicative
import Control.Monad hiding (ap)
import CSV
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

{-
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
-}

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

overlapComponents1 :: OverlapGraph -> [[ItemId]]
overlapComponents1 = map (map snd . labNodes) . filter ((>1) . noNodes) . SG.gComponents

overlapComponents2 :: OverlapGraph -> [[ItemId]]
overlapComponents2 = 
  map (map snd . labNodes) .  
  concatMap (SG.splitToSize 7) . filter ((>1) . noNodes) . SG.gComponents

type OverlapComponent = (Int, [(ItemId, [ItemId], [Int])])

maxComponentSize = 10

overlapComponents2' :: OverlapGraph -> [OverlapComponent]
overlapComponents2' og = 
  [ (i, map (mkUnit i) $ nodes oc) | (i,oc) <- ocs ]
  where ocs  = zip [1..] . concatMap (SG.splitToSize maxComponentSize) . 
               filter ((>1) . noNodes) $ SG.gComponents og
        gi x = map fst $ filter ((x `gelem`) . snd) ocs
        mkUnit i x = (fromJust $ lab og x, 
                      mapMaybe (lab og) . nub $ neighbors og x, 
                      filter (/=i) $ gi x)

analyseOverlaps = do
  og <- loadOverlapGraph
  Right cat <- loadCatalogue catFile
  let  oc  = filter ((>1) . noNodes) $ SG.gComponents og
       n   = sum $ map noNodes oc
       xs  = C.fromList $ map noNodes oc
       oc' = concatMap (SG.splitToSize maxComponentSize) oc
       n'  = sum $ map noNodes oc'
       xs' = C.fromList $ map noNodes oc'
       pl  = C.fromList $ map longestShortestPath oc'
       a   = length . nub $ concatMap nodes oc' \\ concatMap nodes oc
       zs  = C.fromList . map snd . C.counts . C.fromList $ concatMap nodes oc'
       gs  = mkGroups cat $ overlapComponents2' og
       ges = C.fromList $ map (length . snd) gs
       gcs = C.fromList $ map (length . fst) gs
  putStr . unlines $
    [ printf "%d non-singleton components" (length oc) 
    , printf "%d nodes in non-singletons" n 
    , printf "components size histogram: %s" (show $ C.counts xs) 
    , printf "size-%d-splitted components: %d" maxComponentSize (length oc')
    , printf "components size histogram: %s" (show $ C.counts xs')
    , printf "longest shortest paths histogram: %s" (show $ C.counts pl)
    , printf "articulation points splitted: %d" a
    , printf "node multiplicity histogram: %s" (show $ C.counts zs)
    , printf "number of groups: %d" (length gs)
    , printf "group sizes (by components) histogram: %s" (show $ C.counts gcs)
    , printf "group sizes (by editors) histogram: %s" (show $ C.counts ges) ]

longestShortestPath :: Gr a b -> Int
longestShortestPath g = maximum $ map (longest g) (nodes g)
  where longest g x = maximum . map (\(LP p) -> length p) . spTree x $ emap (const 1) g

mkGroups :: Catalogue -> [OverlapComponent] -> [([OverlapComponent], [ItemEditor])]
mkGroups cat oc = 
  [ (map fst xs, e) | xs@((x,e):_) <- eqClasses ((==) `on` snd) xss ]
  where xss = map (\x -> (x, componentEditors cat x)) oc

componentEditors :: Catalogue -> OverlapComponent -> [ItemEditor]
componentEditors cat (_,zs) =
  nub . sort $ concatMap (\(xs,_,_) -> itemEditors . fromJust $ getItem cat xs) zs

csvOverlapComponent :: Catalogue -> OverlapComponent -> CSV 
csvOverlapComponent c oc@(i,zs) = 
  [printf "COMPONENT %d" i] : [printf "EDITORS: %s" editors] : [w] : [] :
  concatMap csvUnit zs
  where csvUnit (x,o,z) = 
          let r = if null z then
                    printf "Overlaps with: %s" (intercalate ", " $ map showItemId o) 
                  else
                    printf "REPEATED IN COMPONENTS: %s / Overlaps with KUs: %s" 
                      (intercalate ", " $ map show z)
                      (intercalate ", " $ map showItemId o) 
          in (++[[]]) . addRemark r . fromJust $ csvKnowledgeUnit c x
        editors = intercalate ", " $ componentEditors c oc
        hasRepeated = any (\(_,_,xs) -> not $ null xs) zs
        w = if hasRepeated then "WARNING: This component shares some KUs with other components (see remarks in column I)" else ""
        

addRemark :: String -> CSV -> CSV
addRemark r (h:x:xs) = h : (x ++ [r]) : xs

-- todo: merge OverlapComponents with Catalogue, so that we have overlap links there in the catalogue!

main = do
  og <- loadOverlapGraph
  Right c <- loadCatalogue catFile
  let oc  = overlapComponents2' og
      csv = concatMap (csvOverlapComponent c) oc 
  return $ showCSV csv
--  return $ mkGroups c oc

