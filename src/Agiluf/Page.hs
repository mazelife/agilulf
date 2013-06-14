module Page  (publish) where


import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as LZ
import Data.Data
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)
import Text.Hastache
import Text.Hastache.Context
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Readers.RST (readRST)

import Config
import Definition
import Entry (getSortedEntries)
import Navigation (getEntryNavigation, paginate)
import Pandoc (readEntries)
import File
import Tags


paginateEntries :: IO [Entry] -> IO [EntryPage]
paginateEntries entries = fmap paginate entries
    where makePage (entry, nav) = EntryPage entry nav
          paginate entries = (map makePage (zip entries (getEntryNavigation entries)))


renderPage :: String -> EntryPage -> IO ()
renderPage template page = hastacheStr defaultConfig (encodeStr template) context >>= writePost
    where context = mkGenericContext page
          writePost = LZ.writeFile (get_page_path $ fileName $ entry page)


paginateIndex :: IO [Entry] -> IO Blog -> IO [IndexPage]
paginateIndex es b = do
    entries <- es
    blog <- b
    let entries_per_page = entriesPerPage blog
    let makePage (nav, entries, num) = IndexPage entries nav num
    return $ map makePage (paginate entries_per_page entries)


renderIndex :: String -> IndexPage -> IO ()
renderIndex template page = hastacheStr defaultConfig (encodeStr template) context >>= writeIndex
    where context = mkGenericContext page
          slug = if number page == 1 then "index.html" else "page" ++ (show $ number page) ++ ".html"
          writeIndex = LZ.writeFile (get_page_path slug)


renderTagIndex :: String -> TagPage -> IO ()
renderTagIndex template page = hastacheStr defaultConfig (encodeStr template) context >>= writeIndex
    where context = mkGenericContext page
          slug = tagHref $ tag page
          writeIndex = LZ.writeFile (get_page_path slug)


publish blog = do

    createOutputDirectories blog
    copyStaticFiles blog

    blogConf <- blog
    let entry_directory = joinPath [(baseDirectory blogConf), "entries"]
    let es = getSortedEntries entry_directory blogConf

    entry_pages <-  paginateEntries es
    entry_template_file <- readFile $ entryTemplate blogConf
    mapM (renderPage entry_template_file) entry_pages

    index_pages <- paginateIndex es blog
    index_template_file <- readFile $ indexTemplate blogConf
    mapM (renderIndex index_template_file) $ index_pages

    tag_pages <- paginateTagIndex es
    tag_template_file <- readFile $ tagTemplate blogConf
    mapM (renderTagIndex tag_template_file) $ tag_pages
    putStrLn "All done!"
