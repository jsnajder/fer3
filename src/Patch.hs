
module Patch where

import Catalogue
import Control.Applicative
import Control.Monad
import CSV
import Data.List
import Data.Maybe
import qualified Data.Set as S

type Patch = CatalogueComponent

data Diff = Diff
  { removed  :: [ItemId]
  , added    :: [ItemId]
  , modified :: [ItemId]
  , replaced :: [(ItemId,ItemId)] } deriving (Eq,Ord,Show)

patchDiff :: Catalogue -> Patch -> Diff
patchDiff cat (cmp,links) = Diff
  { removed  = removed
  , added    = added
  , modified = modified 
  , replaced = replaced }
  where catItems  = knowledgeUnits cat
        cmpItems  = knowledgeUnits cmp
        catItems' = map itemId catItems
        cmpItems' = map itemId cmpItems
        hit       = cmpItems' `intersect` catItems'
        new       = cmpItems' \\ catItems'
        retained  = pointedTo `intersect` cmpItems'
        added     = new `intersect` retained
        removed   = (hit \\ retained) \\ map fst replaced
        replaced  = filter (\(x1,x2) -> x1/=x2) $ 
                    map (\(x1,x2,_) -> (x1,x2)) replaceLinks
        modified  = filter (not . fromJust . identicalItems cat cmp) (retained `intersect` hit)
        replaceLinks = filter (\(x1,x2,l) -> l==ReplacedBy) links
        pointedTo = map (\(_,x,_) -> x) replaceLinks

-- whether two items are identical up to remark fields
identicalItems :: Catalogue -> Catalogue -> ItemId -> Maybe Bool
identicalItems c1 c2 x = liftA2 (==) (f <$> getItemTree c1 x) (f <$> getItemTree c2 x)
  where f = fmap (\x -> x { itemRemark = Nothing })

patch :: Catalogue -> Patch -> (Catalogue, Diff)
patch c p@(cmp,_) = (c4, d)
  where d  = patchDiff c p
        c2 = removeItems' c $ removed d
        adds = map fixTreeItemIds . treesWithItems cmp $ added d ++ modified d
        c3 = foldl' addItemTree c2 adds
        c4 = fixItemIds $ pruneItems c3  -- CHECK: fixItemIds not working?
        --Just c5 = foldM (\c (x,y) -> replaceItem' c x y) c3 $ replaced p

-- TODO: purify overlap links of resolved units
-- TODO: combine editors
-- TODO: KA/KT mapping detection (warrning)

-- for all resolved items (== ReplaceBy sources),
-- remove all overlap links to items within this component
purifyOverlapLinks :: Patch -> Patch
purifyOverlapLinks = undefined

-- for all ReplaceBy targets, take ReplaceBy source editors
combineEditors :: Patch -> Patch
combineEditors = undefined

-- patch is bogus if maps from/to KA/KT (only KU mappings are allowed)
bogusPatch :: Patch -> Bool
bogusPatch = undefined
 
main = do
  Right c <- loadCatalogue "../data/catalogue/v0.2/catalogue/FER3-v0.2.3.csv"
  Right p <- loadCatalogueComponent "../data/catalogue/v0.2/components-resolved-csv/g020-c022-proba.res.csv"
  let (c',diff) = patch c p
  saveCatalogue "../data/catalogue/v0.2/catalogue/patched.csv" c'
  return diff

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
