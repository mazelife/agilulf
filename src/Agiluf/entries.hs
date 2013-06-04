module Entries ( getRSTFiles
               , getSlug
               ) where


import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))


-- | Get the list of RST files in the directory.
getEntryNames :: FilePath -> IO ()
getEntryNames dir = do
    contents <- getDirectoryContents dir
    let x = getRSTFiles dir contents
    putStr $ show x
    putStr "\n"

-- | Given a filepath and list of files in it, return the path
-- of all RST files.
getRSTFiles :: FilePath -> [String] -> [FilePath]
getRSTFiles dir directory_contents = map (\n -> dir </> n) files
    where files = filter isRSTFile directory_contents

-- | Get the slug name (the filename without the extension)
getSlug :: String -> String
getSlug filename = take ((length filename) - 4) filename

-- | Checks the extension of the file name.
isRSTFile :: String -> Bool
isRSTFile name = ext == ".rst" && name /= ext
    where ext = (drop ((length name) - 4) name)