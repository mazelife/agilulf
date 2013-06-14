import System.Environment

import Config (getBlog)
import File (getBlogPath, getConfig)
import Page (publish)


main :: IO ()
main = publish blog
    where blog = getBlog $ getBlogPath >>= getConfig

