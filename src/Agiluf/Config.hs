module Config where


import Data.Default (def)
import System.FilePath (joinPath)
import Text.Pandoc.Options


-- | The base URL of the website
base_url = "http://www.example.com"
-- | The directory where the project is located
base_directory = "/Users/jstevenson/src/mazelife"
-- | The directory where blog entries are located
entry_directory = joinPath [base_directory, "entries"]
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
-- | The directory where the static site should be published
output_directory = joinPath [base_directory, "output"]
-- | The path to the JSON index
json_path =  joinPath [output_directory, "index.json"]
-- | How dates shoudl be formatted.
date_format = "%B %e, %Y %l:%M %p"
-- | Number of recent posts to show in RSS
rss_limit = 10
-- | The name of the website
site_name = "My WebSite"
-- | Site description
site_description = "A description of my website"

-- | Get the full path for a blog entry given a page slug.
get_page_path:: String -> String
get_page_path slug = let filename = slug ++ ".html"
                     in  joinPath [output_directory, filename]


-- | Default Pandoc reader options.
reader_options = def ReaderOptions

-- | Default Pandoc writer options.
writer_options = def WriterOptions