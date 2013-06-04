{-# LANGUAGE DeriveDataTypeable #-}


module Definition where


import Data.Data
import Data.Time
import Data.Typeable
import Text.Pandoc.Definition (Pandoc)


data Entry = Entry { doc :: Pandoc
                   , title :: String
                   , date :: UTCTime
                   , displayDate :: String
                   , authors :: [String]
                   , description :: String
                   , slug :: String
                   , tags :: [String]
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


data TagPage = TagPage { tagEntries :: [Entry]
                       , tagIndexNavigation :: IndexNavigation
                       , tagNumber :: Int
                       , tag :: String
                       } deriving (Show, Data, Typeable)


data RSSDoc = RSSDoc { rssEntries :: [Entry]
                     , rssTitle :: String
                     , rssDescription :: String
                     , rssLink :: String
                     , lastUpdated :: String
                     } deriving (Show, Data, Typeable)
