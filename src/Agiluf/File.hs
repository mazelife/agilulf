module File where


import System.Cmd
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Exit
import System.FilePath (joinPath)

import Definition


{--

    Useful utilities for doing filesystems tasks and checks.

--}


verifyBlogStructure :: FilePath -> IO Bool
verifyBlogStructure blog_directory = do
    config_exists <- doesFileExist $ joinPath [blog_directory, "config"]
    root_exists <- checkFolder ""
    enties_exist <- checkFolder "entries"
    templates_exist <- checkFolder "templates"
    if not root_exists
        then error "Blog directory is invalid."
        else if not config_exists
            then error ("No configuration file found.")
            else if not enties_exist
                then error "No blog entries folder found."
                else if not templates_exist
                    then error "No templates folder found."
                    else return True
    where checkFolder = blogFolderExists blog_directory


blogFolderExists :: FilePath -> FilePath -> IO Bool
blogFolderExists root folder = doesDirectoryExist $ joinPath [root, folder]


-- | FIXME: Unix/Mac only.
copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory src dest = system $ "cp -r " ++ src ++ " " ++ dest


-- | Create initial output directories.
createOutputDirectories :: FilePath -> IO ()
createOutputDirectories output_directory = do
    createDirectoryIfMissing False output_directory
    createDirectoryIfMissing False (joinPath [output_directory, "tags"])


-- | Copy statoc files over to the output directory
copyStaticFiles :: FilePath -> FilePath -> IO ExitCode
copyStaticFiles static_directory output_directory = copyDirectory static_directory output_directory