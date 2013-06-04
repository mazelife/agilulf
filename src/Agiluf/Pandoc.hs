module Pandoc where

import Control.Applicative
import Data.Char (toLower)
import Data.List (unfoldr)
import System.Directory (getDirectoryContents)
import System.FilePath (joinPath)
import Text.Pandoc.Definition
import Text.Pandoc.Readers.RST (readRST)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.HTML (writeHtmlString)


import Config (reader_options, writer_options)


-- | Functions for opening and parsing RST files with Pandoc.


-- | Does the given file path have an .rst extension?
isRSTFile :: String -> Bool
isRSTFile name = ext == ".rst" && name /= ext
    where ext = drop (length name - 4) name


-- | List files in the given directory with an .rst extension.
rstFiles :: FilePath -> IO [FilePath]
rstFiles dir = getRSTPaths dir <$>  getDirectoryContents dir
    where getRSTPaths dir files = [joinPath [dir, f] | f <- files, isRSTFile f ]


-- | Get the contents of the given list of .rst file paths.
getRSTContents :: IO [FilePath] -> IO [String]
getRSTContents paths = mapM readFile =<< paths


-- | Parse a list of .rst file contents into pandoc objects.
parseRSTContents :: IO [String] -> IO [Pandoc]
parseRSTContents = fmap $ map (readRST reader_options)


readEntries :: FilePath -> IO [Pandoc]
readEntries = parseRSTContents . getRSTContents . rstFiles


-- | Functions for extracting data from Pandoc objects.


getDocAuthors :: Pandoc -> [String]
getDocAuthors (Pandoc (Meta _ authors' _) blocks) = map stringify authors'


getDocBlock :: Pandoc -> [Block]
getDocBlock (Pandoc (Meta _ _ _) blocks) = blocks


getDocDate :: Pandoc -> String
getDocDate (Pandoc (Meta _ _ date') blocks) = stringify date'

-- Retrieve all definition list, flatten them and return the result
-- as a list of term/defintion tuples.
getDocDefinitionList :: Pandoc -> [(String, String)]
getDocDefinitionList document = map extractDefinitions (extract dl)
  where
    dl = head $ filter isDefinitionList (getDocBlock document)
    extract (DefinitionList d) = d


getDocDescription :: Pandoc -> String
getDocDescription doc = snd $ last desc
  where def_list = getDocDefinitionList doc
        desc = filter (\p -> fst p == "description") def_list


getDocHtml :: Pandoc -> String
getDocHtml = writeHtmlString writer_options . removeInitialDefinitionList


getDocSlug :: Pandoc -> String
getDocSlug doc = if null slug then error ("Entry \"" ++ getDocTitle doc ++ "\" is missing a slug field.") else snd $ last slug
  where def_list = getDocDefinitionList doc
        slug = filter (\p -> fst p == "slug") def_list


getDocTags :: Pandoc -> [String]
getDocTags doc = splitByDelimiter '|' (snd $ last tags)
  where def_list = getDocDefinitionList doc
        tags = filter (\p -> fst p == "tags") def_list
        splitByDelimiter :: Char -> String -> [String]
        splitByDelimiter = unfoldr . splitSingle
        splitSingle :: Char -> String -> Maybe (String,String)
        splitSingle _ [] = Nothing
        splitSingle delimiter xs =
          let (ys, zs) = break (== delimiter) xs in
          Just (ys, drop 1 zs)



getDocTitle :: Pandoc -> String
getDocTitle (Pandoc (Meta title' _ _) blocks) = stringify title'



-- | Functions for handling extraneous DefinitionList nodes in Pandoc objects.


-- Is the given block a definition list?
isDefinitionList :: Block -> Bool
isDefinitionList (DefinitionList _) = True
isDefinitionList _ = False


-- | If the document starts with a definiton list, it can be safely stripped as it's just metadata that's
-- | already been extracted. Otheriwse, the doc is left as-is.
removeInitialDefinitionList :: Pandoc -> Pandoc
removeInitialDefinitionList (Pandoc meta blocks) =
  if isDefinitionList $ head blocks
    then Pandoc meta (tail blocks)
    else Pandoc meta blocks


extractDefinitions :: ([Inline], [[Block]]) -> (String, String)
extractDefinitions (iline, block) = (term , def)
  where term = map toLower (stringify iline)
        extract (Para p) = stringify p
        flatten :: [[a]] -> [a]
        flatten [] = []
        flatten (x:xs) = x ++ flatten xs
        def = flatten (map extract (concat block))