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
import Static
import Tags



paginateEntries :: IO [Entry] -> IO [EntryPage]
paginateEntries es = do
    entries <- es
    let makePage (entry, nav) = EntryPage entry nav
    return (map makePage (zip entries (getEntryNavigation entries)))


renderPage :: String -> EntryPage -> IO ()
renderPage template page = do
    let context = mkGenericContext page
    let writePost = LZ.writeFile (get_page_path $ fileName $ entry page)
    hastacheStr defaultConfig (encodeStr template) context >>= writePost


paginateIndex :: IO [Entry] -> IO [IndexPage]
paginateIndex es = do
    entries <- es
    let makePage (nav, entries, num) = IndexPage entries nav num site_name site_description
    return $ map makePage (paginate 2 entries)


renderIndex :: String -> IndexPage -> IO ()
renderIndex template page = do
    let context = mkGenericContext page
    let slug = if number page == 1 then "index.html" else "page" ++ (show $ number page) ++ ".html"
    let writeIndex = LZ.writeFile (get_page_path slug)
    hastacheStr defaultConfig (encodeStr template) context >>= writeIndex


renderTagIndex :: String -> TagPage -> IO ()
renderTagIndex template page = do
    let context = mkGenericContext page
    let slug = tagHref $ tag page
    let writeIndex = LZ.writeFile (get_page_path slug)
    hastacheStr defaultConfig (encodeStr template) context >>= writeIndex


publish path = do

    createOutputDirectories output_directory
    copyStaticFiles static_directory output_directory

    let es = getSortedEntries path

    entry_pages <-  paginateEntries es
    entry_template_file <- readFile entry_template
    mapM (renderPage entry_template_file) entry_pages

    index_pages <- paginateIndex es
    index_template_file <- readFile index_template
    mapM (renderIndex index_template_file) $ index_pages

    tag_pages <- paginateTagIndex es
    tag_template_file <- readFile tag_template
    mapM (renderTagIndex tag_template_file) $ tag_pages

    putStrLn "All done!"
