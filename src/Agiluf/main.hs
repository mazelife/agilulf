import Control.Monad
import System.Environment

import Config (getBlog)
import File (getBlogPath, getConfig, postPublishHook)
import Page (publish)


main :: IO ()
main = publish blog >> postPublishHook conf
    where conf = getBlogPath >>= getConfig
          blog = getBlog $ conf
