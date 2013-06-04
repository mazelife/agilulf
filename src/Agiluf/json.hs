module JSON ( Post ) where


-- | Record type for JSON serialization.
data Post = Post { title :: String
                 , date :: String
                 , url :: String
                 , description :: String
                 , authors :: [String]
                 , tags :: [String]
                 } deriving (Show)
