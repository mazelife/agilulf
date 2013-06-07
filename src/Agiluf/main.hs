import System.Environment

import Page (publish)
import File (verifyBlogStructure)

main = do
    (blog_path:_) <- getArgs
    verifyBlogStructure blog_path
    publish "entries"