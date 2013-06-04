module RSS where


import Config
import Definition


feed :: IO [Entry] -> IO RSSDoc
feed es = do
    entries <- es
    let last_entries = take rss_limit entries
    return $ RSSDoc last_entries site_name site_description base_url (displayDate $ head entries)