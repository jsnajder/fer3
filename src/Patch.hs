
module Patch where

import Catalogue
import Control.Applicative
import CSV
import Data.List
import Data.Maybe
import qualified Data.Set as S

type Patch = CatalogueComponent

data Changes = Changes
  { removed  :: [ItemId]
  , added    :: [ItemId]
  , replaced :: [(ItemId,ItemId)]
  , modified :: [ItemId] } deriving (Eq,Ord,Show)

--patch :: Catalogue -> Patch -> (Catalogue, Changes)
patch cat (cmp,links) = Changes
  { removed  = removed
  , added    = added
  , replaced = replaced
  , modified = modified }
  where catUnits  = knowledgeUnits cat
        cmpUnits  = knowledgeUnits cmp
        catUnits' = map itemId catUnits
        cmpUnits' = map itemId cmpUnits
        hit       = cmpUnits' `intersect` catUnits'
        new       = cmpUnits' \\ catUnits'
        retained  = pointedTo `intersect` cmpUnits'
        added     = new `intersect` retained
        removed   = (hit \\ retained) \\ map fst replaced
        replaced  = filter (\(x1,x2) -> x1/=x2) $ map (\(x1,x2,_) -> (x1,x2)) replaceLinks
        modified  = filter (not . fromJust . identicalItems cat cmp) (retained `intersect` hit)
        replaceLinks = filter (\(x1,x2,l) -> l==ReplacedBy) links
        pointedTo = map (\(_,x,_) -> x) replaceLinks

identicalItems :: Catalogue -> Catalogue -> ItemId -> Maybe Bool
identicalItems c1 c2 x = liftA2 (==) (f <$> getItemTree c1 x) (f <$> getItemTree c2 x)
  where f = fmap (\x -> x { itemRemark = Nothing })


main = do
  Right c <- loadCatalogue "../data/catalogue/v0.2/catalogue/FER3-v0.2.3.csv"
  Right p <- loadCatalogueComponent "../data/catalogue/v0.2/components-resolved-csv/g020-c022-proba.res.csv"
  return $ patch c p

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
