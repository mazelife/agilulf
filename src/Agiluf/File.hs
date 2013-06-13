module File where


import Control.Applicative
import Data.Configurator
import Data.Configurator.Types as C
import System.Cmd
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import System.FilePath (joinPath)

import Definition


{--

    Useful utilities for doing filesystems tasks and checks.

--}


-- | Verify the file structure of the specified (in command-line arg) blog directory
-- | and retrieve create a Configurator object containing blog settings.
getConfig :: FilePath -> IO C.Config
getConfig blog_directory = do
    config_exists <- doesFileExist $ config_path
    root_exists <- checkFolder ""
    enties_exist <- checkFolder "entries"
    templates_exist <- checkFolder "templates"
    if not root_exists
        then error "Blog directory is invalid."
        else if not config_exists
            then error "No configuration file found."
            else if not enties_exist
                then error "No blog entries folder found."
                else if not templates_exist
                    then error "No templates folder found."
                    else readConfigFile
    where config_path = joinPath [blog_directory, "config"]
          checkFolder = blogFolderExists blog_directory
          readConfigFile =  load [(Required config_path)]


-- | Get the filepath to the blog from the command line arguments.
getBlogPath :: IO FilePath
getBlogPath = do
    args <- getArgs
    if null args
        then error "You must provide a filepath to the blog directory."
        else return $ head args


-- | Does the specified directory exist within the blog root directory?
blogFolderExists :: FilePath -> FilePath -> IO Bool
blogFolderExists root folder = doesDirectoryExist $ joinPath [root, folder]


-- | FIXME: Unix/Mac only.
copyDirectory :: FilePath -> FilePath -> IO ExitCode
copyDirectory src dest = system $ "cp -r " ++ src ++ " " ++ dest


-- | Create initial output directories.
createOutputDirectories :: IO Blog -> IO ()
createOutputDirectories blog = do
    blogConf <- blog
    let output_directory = outputDirectory blogConf
    createDirectoryIfMissing False output_directory
    createDirectoryIfMissing False (joinPath [output_directory, "tags"])


-- | Copy static files over to the output directory
copyStaticFiles :: IO Blog -> IO ExitCode
copyStaticFiles blog = do
    blogConf <- blog
    copyDirectory (staticDirectory blogConf) (outputDirectory blogConf)





