module RSS (getRSSDoc, renderRSS) where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LZ
import Data.Time
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Text.Hastache
import Text.Hastache.Context


import Config
import Definition
import Entry (formatDate)




getRSSDoc :: IO UTCTime -> IO Blog -> IO [Entry] -> IO RSSDoc
getRSSDoc = liftA3 feed


feed :: UTCTime -> Blog -> [Entry] -> RSSDoc
feed now blog entries =
    let limit = rssLimit blog
        name = siteName blog
        desc = metaDescription blog
        url = baseUrl blog
        updated = rssDate $ date $ head entries
        built = rssDate now
    in RSSDoc (take limit entries) name desc url updated built



renderRSS :: Blog -> String -> RSSDoc -> IO ()
renderRSS blog template rssDoc = hastacheStr defaultConfig (encodeStr template) context >>= writeFeed
    where context = mkGenericContext rssDoc
          writeFeed = LZ.writeFile $ get_page_path blog "blog.rss"


rssDate :: UTCTime -> String
rssDate = formatTime defaultTimeLocale "%B %e, %Y %l:%M %p"
