import System.Environment

import Config (getBlog)
import File (getBlogPath, getConfig)
import Page (publish)


main :: IO ()
main = do
    blog_path <- getBlogPath
    let blog = getBlog $ getConfig blog_path
    publish blog