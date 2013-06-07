module Entry where


import Control.Applicative ((<$>))
import Control.Monad (msum)
import Data.Data
import Data.List (sortBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Time
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.IO
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Readers.RST (readRST)
import Text.Pandoc.Writers.HTML (writeHtml , writeHtmlString)

import Config (date_format)
import Definition
import Pandoc
import Tags (getTag)


parseDatestamp :: String -> UTCTime
parseDatestamp input_string = timeFromJust time
  where possible_formats = ["%F %T", "%D %T"]
        time = msum $ map (\f -> parseTime defaultTimeLocale f  input_string :: Maybe UTCTime) possible_formats
        timeFromJust :: Maybe a -> a
        timeFromJust Nothing = error $ "Invalid datestring: " ++ input_string
        timeFromJust (Just x) = x


formatDate:: UTCTime -> String
formatDate t = formatTime defaultTimeLocale format_string t
    where format_string = date_format


getEntry :: Pandoc -> Entry
getEntry doc = Entry doc _title _date _displayDate _authors _description _fileName _tags _html
    where _title       = getDocTitle doc
          _date        = parseDatestamp $ getDocDate doc
          _displayDate = formatDate _date
          _authors     = getDocAuthors doc
          _description = getDocDescription doc
          _fileName    = getDocFilename doc
          _tags        = getDocTags doc
          _html        = getDocHtml doc


-- | Convert a list of pandocs to entry/meta pairs
getEntries :: IO [Pandoc] -> IO [Entry]
getEntries pandocs = (map getEntry) <$> pandocs


-- | Sort a list of entries by date.
sortEntries :: IO [Entry] -> IO [Entry]
sortEntries entries = (sortBy compareEntries) <$> entries


-- | Compare two of entry/meta pairs on their date, descending.
compareEntries :: Entry -> Entry -> Ordering
compareEntries a b = case (date a) `compare` (date b) of
    EQ -> EQ
    GT -> LT
    LT -> GT


getSortedEntries = sortEntries . getEntries . readEntries


