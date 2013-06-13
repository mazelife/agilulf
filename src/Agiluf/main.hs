import System.Environment

import Config (getBlog)
import File (getBlogPath, getConfig)
import Page (publish)


main :: IO ()
main = do
    let blog = getBlog $ getBlogPath >>= getConfig
    publish blog
