{-# LANGUAGE DeriveDataTypeable #-}


module Definition where


import Data.Data
import Data.Time
import Data.Typeable
import System.FilePath (FilePath)
import Text.Pandoc.Definition (Pandoc)


data Blog = Blog { baseUrl :: String
                 , siteName :: String
                 , metaDescription :: String
                 , entriesPerPage :: Int
                 , baseDirectory :: FilePath
                 , entryTemplate :: FilePath
                 , indexTemplate :: FilePath
                 , tagTemplate :: FilePath
                 , outputDirectory :: FilePath
                 , staticDirectory :: FilePath
                 , dateFormat :: String
                 , rssLimit :: Int
                 } deriving (Show, Data, Typeable)


data Entry = Entry { doc :: Pandoc
                   , title :: String
                   , date :: UTCTime
                   , displayDate :: String
                   , authors :: [String]
                   , description :: String
                   , fileName :: String
                   , tags :: [Tag]
                   , image :: String
                   , html :: String
                   } deriving (Show, Data, Typeable)


data EntryNavigation = EntryNavigation { previousEntry :: Bool
                                       , previousEntryHref :: String
                                       , previousEntryLabel :: String
                                       , nextEntry :: Bool
                                       , nextEntryHref :: String
                                       , nextEntryLabel :: String
                                       } deriving (Show, Data, Typeable)


data EntryPage = EntryPage { entry :: Entry
                           , entryNavigation :: EntryNavigation
                           } deriving (Show, Data, Typeable)


data IndexNavigation = IndexNavigation { currentIndex :: String
                                       , previousIndex :: Bool
                                       , previousIndexHref :: String
                                       , nextIndex :: Bool
                                       , nextIndexHref :: String
                                       } deriving (Show, Data, Typeable)


data IndexPage = IndexPage { entries :: [Entry]
                           , indexNavigation :: IndexNavigation
                           , number :: Int
                           } deriving (Show, Data, Typeable)


data Tag = Tag { tagName :: String
               , tagHref :: String
               } deriving (Show, Data, Typeable)


data TagPage = TagPage { tag :: Tag
                       , tagEntries :: [Entry]
                       } deriving (Show, Data, Typeable)


data RSSDoc = RSSDoc { rssEntries :: [Entry]
                     , rssTitle :: String
                     , rssDescription :: String
                     , rssLink :: String
                     , lastUpdated :: String
                     } deriving (Show, Data, Typeable)
