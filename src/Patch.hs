
module Patch where

import Catalogue
import Control.Applicative
import Control.Monad
import CSV
import Data.List
import Data.Maybe
import qualified Data.Set as S
import System.Directory
import System.Environment
import System.FilePath

import Debug.Trace

type Patch = CatalogueComponent

data Diff = Diff
  { removed  :: [ItemId]
  , added    :: [ItemId]
  , modified :: [ItemId]
  , retained :: [ItemId]
  , replaced :: [(ItemId,ItemId)] } deriving (Eq,Ord,Show)

patchDiff :: Catalogue -> Patch -> Diff
patchDiff cat (cmp,links) = Diff
  { removed  = removed
  , added    = added
  , modified = modified
  , retained = retained
  , replaced = replaced }
  where catItems  = knowledgeUnits cat
        cmpItems  = knowledgeUnits cmp
        catItems' = map itemId catItems
        cmpItems' = map itemId cmpItems
        hit       = cmpItems' `intersect` catItems'
        new       = cmpItems' \\ catItems'
        kept       = pointedTo `intersect` cmpItems'
        added     = new `intersect` kept
        removed   = (hit \\ kept) \\ map fst replaced
        replaced  = filter (uncurry (/=)) replaceLinks
        retained  = map fst $ filter (uncurry (==)) replaceLinks
        modified  = filter (not . fromJust . identicalItems cat cmp) $
                    kept `intersect` hit
        replaceLinks = map (\(x1,x2,_) -> (x1,x2)) $
                       filter (\(x1,x2,l) -> l==ReplacedBy) links
        pointedTo = map snd replaceLinks

-- whether two items are identical up to remark fields and topic editors
identicalItems :: Catalogue -> Catalogue -> ItemId -> Maybe Bool
identicalItems c1 c2 x = liftA2 (==) (f <$> getItemTree c1 x) (f <$> getItemTree c2 x)
  where f = fmap (\x -> x { itemRemark = Nothing, 
                            itemEditors = if itemType x == KT then [] 
                                            else itemEditors x  })

patch :: Catalogue -> Patch -> (Catalogue, Diff)
patch c p@(cmp,_) = (c6, d)
  where d  = patchDiff c p
        c2 = removeItems' c $ removed d
        adds = map fixTreeTopicIds . treesWithItems cmp $ added d ++ modified d
        c3 = foldl' addItemTree c2 adds
        overlaps = overlapLinks c3
        resolved = retained d ++ map fst (replaced d)
        overlaps2 = filter (\(x1,x2,_) -> 
                      x1 `elem` resolved && x2 `elem` units) overlaps
        units = map itemId $ knowledgeUnits cmp
        c4 = foldl' removeLink c3 overlaps2
        c5 = combineEditorsAndOverlaps c4 p
        c6 = foldl' (\c (x1,x2) -> fromMaybe c (replaceItem' c x1 x2)) c5 $ replaced d
--        c6 = fixItemIds $ pruneItems c5  -- DO THIS AFTER APPLYING ALL PATCHES!

overlapLinks :: Catalogue -> [(ItemId,ItemId,Link)]
overlapLinks = filter (\(_,_,l) -> l==Overlaps) . links'

-- for all resolved items (== ReplaceBy sources),
-- remove all overlap links to items within this component
purifyOverlapLinks :: Catalogue -> Patch -> Patch
purifyOverlapLinks c p = undefined

-- for all ReplaceBy targets, take ReplaceBy source editors
-- (remark: inefficient, because diff is computed again)
combineEditorsAndOverlaps :: Catalogue -> Patch -> Catalogue
combineEditorsAndOverlaps c p@(cmp,_) = c3
  where Diff rem add mod ret rep = patchDiff c p
        rep' = mapMaybe (\(x1,x2) -> 
                 liftA2 (,) (getItem c x1) (getItem cmp x2)) rep
        addEditors x1 x2 = x2 {
            itemEditors = sort . nub $ itemEditors x1 ++ itemEditors x2 }
        links' = substitute rep $ overlapLinks c
        c2 = foldl' (\c (x1,x2) -> modifyItem c (itemId x2) (addEditors x1)) c rep'
        c3 = fromJust $ addLinks c2 links'
        substitute zs xs = [(fromMaybe x1 $ lookup x1 zs,x2,l) | (x1,x2,l) <- xs]
 
-- patch is bogus if maps from/to KA/KT (only KU mappings are allowed)
-- OR if it resolves a unit that does not exist anymore or maps to
-- a unit that does not exist in the component or in the catalogue
bogusPatch :: Catalogue -> Patch -> Bool
bogusPatch c p@(cmp,links) = 
  (mod `intersect` catItems /= mod) ||
  (repFrom `intersect` catItems /= repFrom) ||
  (repTo `intersect` (catItems ++ cmpItems) /= repTo) ||
  any (\x -> itemType' x == KA || itemType' x == KT) linked 
  where catItems  = map itemId $ knowledgeItems c
        cmpItems  = map itemId $ knowledgeItems cmp
        Diff rem add mod ret rep = patchDiff c p
        (repFrom, repTo) = unzip rep
        linked = concatMap (\(x1,x2,_) -> [x1,x2]) $
                 filter (\(x1,x2,l) -> l==ReplacedBy) links

patchAndLog :: Catalogue -> FilePath -> IO (Catalogue,CSV)
patchAndLog c f = do
  Right p <- loadCatalogueComponent f
  let (c2,d) = patch c p
      Diff rem add mod ret rep = d
      xs = [[patchFile,showItemId x,"Removed"] | x <- rem] ++ 
           [[patchFile,showItemId x,"Added"] | x <- add] ++ 
           [[patchFile,showItemId x,"Modified"] | x <- mod] ++ 
           [[patchFile,showItemId x,"Retained"] | x <- ret] ++ 
           [[patchFile,showItemId x,"Replaced by " ++ showItemId y] | (x,y) <- rep]
  return (c2,xs)
  where patchFile = takeFileName f
 
main = do
  Right c <- loadCatalogue "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.3/FER3-v0.2.6.csv"
  Right p <- loadCatalogueComponent "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/components-resolved-csv/ok/g115-c288.res.csv"
  let (_,diff) = patch c p
  return diff

dInbox = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/components-resolved-csv/inbox"
catFile = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.3/FER3-v0.2.7.csv"
dOk = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/components-resolved-csv/ok"
dBogus = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/components-resolved-csv/bogus"
newCatFile = "FER3-v0.2.8.csv"

sortPatches1 = do
  Right c <- loadCatalogue catFile
  fs <- filter (".csv" `isSuffixOf`) <$> getDirectoryContents dInbox
  forM_ fs $ \f -> do
    putStr $ f ++ ": "
    Right p <- loadCatalogueComponent $ dInbox </> f
    putStrLn $ if bogusPatch c p then "BOGUS" else "OK"
    if bogusPatch c p 
      then renameFile (dInbox </> f) (dBogus </> f)
       else renameFile (dInbox </> f) (dOk </> f)

sortPatches2 = do
  Right c <- loadCatalogue catFile
  fs <- filter (".csv" `isSuffixOf`) <$> getDirectoryContents dOk
  forM_ fs $ \f -> do
    putStr $ f ++ ": "
    Right p <- loadCatalogueComponent $ dOk </> f
    let d = patchDiff c p
    if null (replaced d) 
      then do
        putStrLn "NO-REPLACE"
        renameFile (dOk </> f) (dOk </> "no-replace" </> f)
      else do
        putStrLn "REPLACE"
        renameFile (dOk </> f) (dOk </> "replace" </> f)

dOut = "/home/jan/fer3/fer3-catalogue/data/catalogue/v0.2/components-resolved-csv/out"

applyPatches = do
  Right c <- loadCatalogue catFile
  fs <- map (dOk </>) . filter (".csv" `isSuffixOf`) <$> getDirectoryContents dOk
  (cNew,log) <- foldM (\(c,csv) f -> do
    (c2,csv2) <- patchAndLog c f
    putStrLn $ f ++ show (length csv2)
    return (c2,csv++csv2)) (c,[]) fs
  saveCatalogue (dOut </> newCatFile) (removeTopicEditors $ addInfoRemark cNew)
  writeFile (dOut </> "log.csv") (showCSV log)
  
  
   

-- proći kroz stablo, pobrati linkove

-- insert SupercededBy link according to column J
-- 
-- extra pass: fix overlapping IDs
-- extra pass: fix child ids
-- check dangling pointers
-- delete (and log) areas/units/topics with outgoing edges
-- compare units: old version vs new version (output to log)

-- komponente su PATCHES: svaka mijenja underlying katalog
-- granula koja ima nešto u J stupcu se smatra razriješenom U TOJ komponenti: dakle 
--    brišu se in/out overlaping edges
-- o temama još razmisli

-- Korak 1: PRIMIJENI transformaciej untuar komponente 
-- Korak 2: zapatchaj katalog: dodaj nove granule, obriši SupercededBy
