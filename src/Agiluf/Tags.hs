module Tags where


import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Char (isAlphaNum, toLower)
import Data.List (concatMap, nub)
import qualified Data.Map as Map
import System.FilePath (joinPath)


import Config
import Definition
import Navigation


getTagCloud :: IO [Entry] -> IO [String]
getTagCloud entries = (nub . concatMap tagNames) <$> entries
    where tagNames e = map tagName (tags e)


paginateTagIndex :: IO [Entry] -> IO [TagPage]
paginateTagIndex entries = liftM entriesToPages entries
    where entriesToPages es = map (\(t, es) -> TagPage (getTag t) es) (Map.toList $ mapTags es)



mapTags :: [Entry] -> Map.Map String [Entry]
mapTags entries = tagMap (concatMap getTags entries)
    where getTags entry = [((tagName tag), entry) | tag <- tags entry]
          tagMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs


-- | Convert a tag name to its HTML slug.
tagSlug :: String -> String
tagSlug cs = joinPath ["tags", map sub cs] ++ ".html"
    where sub c = if isAlphaNum c then toLower c else '-'


getTag :: String -> Tag
getTag tag = Tag tag (tagSlug tag)