module Config where


import Data.Configurator
import Data.Configurator.Types as C
import Data.Default (def)
import Data.Text as T
import System.FilePath (joinPath)
import Text.Pandoc.Options

import Definition


-- | The base URL of the website
base_url = "http://www.example.com"
-- | The directory where the project is located
base_directory = "."
-- | The number of entries on a page.
entries_per_page = 5
-- | The directory where templates are located
template_directory = joinPath [base_directory, "templates"]
-- | The path to the blog entry template.
entry_template = joinPath [template_directory, "entry.html"]
-- | The path to the blog index template.
index_template = joinPath [template_directory, "index.html"]
-- | The path to the tag index page template.
tag_template = joinPath [template_directory, "tag.html"]
-- | The path to the RSS feed template
rss_template = joinPath [template_directory, "rss.xml"]
-- | The directory where the static site should be published
output_directory = joinPath [base_directory, "output"]
-- | The path to the JSON index
static_directory = joinPath [base_directory, "static"]
-- | How dates shoudl be formatted.
date_format = "%B %e, %Y %l:%M %p"
-- | Number of recent posts to show in RSS
rss_limit = 10
-- | The name of the website
site_name = "My WebSite"
-- | Site description
site_description = "A description of my website"

-- | Get the full path for a blog entry given a page filename.
get_page_path:: Blog -> String -> FilePath
get_page_path blog filename = joinPath [(outputDirectory blog), filename]


-- | Default Pandoc reader options.
reader_options = def ReaderOptions

-- | Default Pandoc writer options.
writer_options = def WriterOptions


getBlog :: IO (FilePath, C.Config) -> IO Blog
getBlog confGroup = do
    (_base_directory, _conf) <- confGroup
    _base_url <- lookupDefault base_url _conf (pack "base_url")
    _site_name <- lookupDefault site_name _conf (pack "site_name")
    _site_description <- lookupDefault site_description _conf (pack "site_description")
    _entries_per_page <- lookupDefault entries_per_page _conf (pack "entries_per_page")
    _date_format <- lookupDefault date_format _conf (pack "date_format")
    _rss_limit <- lookupDefault rss_limit _conf (pack "rss_limit")
    let _template_directory = joinPath [_base_directory, "templates"]
    let _entry_template = joinPath [_template_directory, "entry.html"]
    let _index_template = joinPath [_template_directory, "index.html"]
    let _tag_template = joinPath [_template_directory, "tag.html"]
    let _rss_template = joinPath [_template_directory, "rss.xml"]
    let _output_directory = joinPath [_base_directory, "output"]
    let _static_directory = joinPath [_base_directory, "static"]
    return $ Blog _base_url _site_name _site_description _entries_per_page _base_directory _entry_template _index_template _tag_template _rss_template _output_directory _static_directory _date_format _rss_limit
