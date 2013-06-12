import System.Environment

import Config (getBlog)
import File (getConfig)
import Page (publish)


main :: IO ()
main = do
    putStrLn "Hi!"
    (blog_path:_) <- getArgs
    let blog = getBlog $ getConfig blog_path
    publish blog